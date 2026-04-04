[@@@ocaml.warning "-26-27-32-39"]
open Probml

let mean lst =
  let n = float_of_int (List.length lst) in
  List.fold_left (+.) 0. lst /. n

let finished_values particles =
  List.filter_map (fun p ->
    match p.result with
    | Finished f -> Some f.value
    | Stepped  _ -> None
  ) particles

let simulate_hmm (n : int) (x0 : int) (trans_p : float) (obs_p : float) : int array =
  let x  = ref x0 in
  let ys = Array.make n 0 in
  for i = 0 to n - 1 do
    let dx = if Rng.next () <= trans_p then 1 else 0 in
    x := !x + dx;
    ys.(i) <- Dist.draw (Rng.next ()) (Dist.binomial !x obs_p)
  done;
  ys

(* LinReg *)
let () =
  let xs = List.init 10 (fun i -> float_of_int i) in
  let ys = List.map (fun x -> 3.0 *. x) xs in
  let exec    tr = exec_model_linreg tr (Model (lin_regr_full xs ys)) in
  let advance tr = exec_pf_linreg    tr (Model (lin_regr_full xs ys)) in

  Printf.printf "=== LinReg ===\n";

  let chain = Rng.handle_random (fun () -> ssmh_eff 1000 exec) in
  Printf.printf "LinReg SSMH: E[m] = %.4f (expected ~3.0)  E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((m,_),_,_) -> m) chain))
    (mean (List.map (fun ((_,c),_,_) -> c) chain));

  let chain_im = Rng.handle_random (fun () -> imh_eff 1000 exec) in
  Printf.printf "LinReg IM:   E[m] = %.4f (expected ~3.0)  E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((m,_),_,_) -> m) chain_im))
    (mean (List.map (fun ((_,c),_,_) -> c) chain_im));

  let fin = Rng.handle_random (fun () -> finished_values (mpf_eff 1000 advance)) in
  Printf.printf "LinReg MPF:  E[m] = %.4f (expected ~3.0)  E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map fst fin)) (mean (List.map snd fin));

  let chain_pmh = Rng.handle_random (fun () -> pmh_eff 50 500 advance exec) in
  Printf.printf "LinReg PMH:  E[m] = %.4f (expected ~3.0)  E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((m,_),_,_) -> m) chain_pmh))
    (mean (List.map (fun ((_,c),_,_) -> c) chain_pmh));

  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf_eff 1000 10 advance exec)) in
  Printf.printf "LinReg RMPF: E[m] = %.4f (expected ~3.0)  E[c] = %.4f (expected ~0.0)\n\n"
    (mean (List.map fst fin_rmpf)) (mean (List.map snd fin_rmpf))

(* HMM *)
let () =
  let ys = Rng.handle_random (fun () -> simulate_hmm 20 5 0.2 0.9) in
  Printf.printf "HMM ys = ";
  Array.iter (fun y -> Printf.printf "%d " y) ys;
  Printf.printf "\n%!";

  let model   = Model (hmm_model 20 5 ys) in
  let exec    tr = exec_model_hmm tr model in
  let advance tr = exec_pf_hmm    tr model in

  Printf.printf "=== HMM ===\n";

  let chain = Rng.handle_random (fun () -> ssmh_eff 2000 exec) in
  Printf.printf "HMM SSMH: E[trans_p] = %.4f  E[obs_p] = %.4f\n"
    (mean (List.map (fun ((t,_),_,_) -> t) chain))
    (mean (List.map (fun ((_,o),_,_) -> o) chain));

  let chain_im = Rng.handle_random (fun () -> imh_eff 2000 exec) in
  Printf.printf "HMM IM:   E[trans_p] = %.4f  E[obs_p] = %.4f\n"
    (mean (List.map (fun ((t,_),_,_) -> t) chain_im))
    (mean (List.map (fun ((_,o),_,_) -> o) chain_im));

  let fin = Rng.handle_random (fun () -> finished_values (mpf_eff 2000 advance)) in
  Printf.printf "HMM MPF:  E[trans_p] = %.4f  E[obs_p] = %.4f\n"
    (mean (List.map fst fin)) (mean (List.map snd fin));

  let chain_pmh = Rng.handle_random (fun () -> pmh_eff 200 100 advance exec) in
  Printf.printf "HMM PMH:  E[trans_p] = %.4f  E[obs_p] = %.4f\n"
    (mean (List.map (fun ((t,_),_,_) -> t) chain_pmh))
    (mean (List.map (fun ((_,o),_,_) -> o) chain_pmh));

  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf_eff 2000 10 advance exec)) in
  Printf.printf "HMM RMPF: E[trans_p] = %.4f  E[obs_p] = %.4f\n\n"
    (mean (List.map fst fin_rmpf)) (mean (List.map snd fin_rmpf))

(* COIN FLIP *)

let coin_flip_model (obs : bool list) (cap : #coin_flip_cap) : float =
  let p = cap#sample_float (Dist.beta 1. 1.) (Addr.make ()) in
  List.iter (fun y ->
    let _ = cap#observe_bool (Dist.bernoulli p) (Addr.make ()) y in ()
  ) obs;
  p

let () =
  (* 7 heads out of 10 flips  *)
  let obs = [true; true; true; false; true; true; false; true; false; true] in
  let model   = Model (coin_flip_model obs) in
  let exec    tr = exec_model_coin_flip tr model in
  let advance tr = exec_pf_coin_flip    tr model in

  Printf.printf "=== Coin Flip ===\n";
  Printf.printf "(7 heads out of 10 flips, expected E[p] ~ 0.636)\n%!";

  let chain = Rng.handle_random (fun () -> ssmh_eff 100 exec) in
  Printf.printf "Coin Flip SSMH: E[p] = %.4f\n"
    (mean (List.map (fun (p,_,_) -> p) chain));

  let chain_im = Rng.handle_random (fun () -> imh_eff 100 exec) in
  Printf.printf "Coin Flip IMH:  E[p] = %.4f\n"
    (mean (List.map (fun (p,_,_) -> p) chain_im));

  let fin = Rng.handle_random (fun () -> finished_values (mpf_eff 1500 advance)) in
  Printf.printf "Coin Flip MPF:  E[p] = %.4f\n"
    (mean fin);

  let chain_pmh = Rng.handle_random (fun () -> pmh_eff 25 100 advance exec) in
  Printf.printf "Coin Flip PMH:  E[p] = %.4f\n"
    (mean (List.map (fun (p,_,_) -> p) chain_pmh));

  let fin_rmpf = Rng.handle_random (fun () ->
    finished_values (rmpf_eff 100 10 advance exec)) in
  Printf.printf "Coin Flip RMPF: E[p] = %.4f\n\n"
    (mean fin_rmpf)