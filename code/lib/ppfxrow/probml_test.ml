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

(* ── LinReg ── *)
let () =
  let xs = List.init 10 (fun i -> float_of_int i) in
  let ys = List.map (fun x -> 3.0 *. x) xs in

  (* SSMH *)
  let exec tr = exec_model_linreg tr (Model (lin_regr_full xs ys)) in
  let chain = Rng.handle_random (fun () -> ssmh 1000 exec) in
  Printf.printf "LinReg SSMH: E[m] = %.4f (expected ~3.0)\n"
    (mean (List.map (fun ((m, _), _, _) -> m) chain));
  Printf.printf "LinReg SSMH: E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((_, c), _, _) -> c) chain));

  (* IM *)
  let chain_im = Rng.handle_random (fun () -> im 1000 exec) in
  Printf.printf "LinReg IM:   E[m] = %.4f (expected ~3.0)\n"
    (mean (List.map (fun ((m, _), _, _) -> m) chain_im));
  Printf.printf "LinReg IM:   E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((_, c), _, _) -> c) chain_im));

  (* MPF *)
  let advance tr = exec_pf_linreg tr (Model (lin_regr_full xs ys)) in
  let fin = Rng.handle_random (fun () -> finished_values (mpf 1000 advance)) in
  Printf.printf "LinReg MPF:  E[m] = %.4f (expected ~3.0)\n" (mean (List.map fst fin));
  Printf.printf "LinReg MPF:  E[c] = %.4f (expected ~0.0)\n" (mean (List.map snd fin));

  (* PMH *)
  let chain_pmh = Rng.handle_random (fun () -> pmh 50 500 advance exec) in
  Printf.printf "LinReg PMH:  E[m] = %.4f (expected ~3.0)\n"
    (mean (List.map (fun ((m, _), _, _) -> m) chain_pmh));
  Printf.printf "LinReg PMH:  E[c] = %.4f (expected ~0.0)\n"
    (mean (List.map (fun ((_, c), _, _) -> c) chain_pmh));

  (* RMPF *)
  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 1000 10 advance exec)) in
  Printf.printf "LinReg RMPF: E[m] = %.4f (expected ~3.0)\n" (mean (List.map fst fin_rmpf));
  Printf.printf "LinReg RMPF: E[c] = %.4f (expected ~0.0)\n\n" (mean (List.map snd fin_rmpf))

(* let () =
  let ys = Rng.handle_random (fun () -> simulate_hmm 20 5 0.2 0.9) in
  Printf.printf "HMM ys = ";
  Array.iter (fun y -> Printf.printf "%d " y) ys;
  Printf.printf "\n%!";

  let model = Model (hmm_model 20 5 ys) in
  let exec    tr = exec_model_hmm tr model in
  let advance tr = exec_pf_hmm    tr model in

  (* SSMH *)
  let chain = Rng.handle_random (fun () -> ssmh 5000 exec) in
  Printf.printf "HMM SSMH: E[trans_p] = %.4f (expected ~0.2)\n"
    (mean (List.map (fun ((t, _), _, _) -> t) chain));
  Printf.printf "HMM SSMH: E[obs_p]   = %.4f (expected ~0.9)\n"
    (mean (List.map (fun ((_, o), _, _) -> o) chain));

  (* IM *)
  let chain_im = Rng.handle_random (fun () -> im 5000 exec) in
  Printf.printf "HMM IM:   E[trans_p] = %.4f (expected ~0.2)\n"
    (mean (List.map (fun ((t, _), _, _) -> t) chain_im));
  Printf.printf "HMM IM:   E[obs_p]   = %.4f (expected ~0.9)\n"
    (mean (List.map (fun ((_, o), _, _) -> o) chain_im));

  (* MPF *)
  let fin = Rng.handle_random (fun () -> finished_values (mpf 5000 advance)) in
  Printf.printf "HMM MPF:  E[trans_p] = %.4f (expected ~0.2)\n" (mean (List.map fst fin));
  Printf.printf "HMM MPF:  E[obs_p]   = %.4f (expected ~0.9)\n" (mean (List.map snd fin));

  (* PMH *)
  let chain_pmh = Rng.handle_random (fun () -> pmh 600 250 advance exec) in
  Printf.printf "HMM PMH:  E[trans_p] = %.4f (expected ~0.2)\n"
    (mean (List.map (fun ((t, _), _, _) -> t) chain_pmh));
  Printf.printf "HMM PMH:  E[obs_p]   = %.4f (expected ~0.9)\n"
    (mean (List.map (fun ((_, o), _, _) -> o) chain_pmh));

  (* RMPF *)
  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 1500 10 advance exec)) in
  Printf.printf "HMM RMPF: E[trans_p] = %.4f (expected ~0.2)\n" (mean (List.map fst fin_rmpf));
  Printf.printf "HMM RMPF: E[obs_p]   = %.4f (expected ~0.9)\n\n" (mean (List.map snd fin_rmpf)) 


let simulate_coin_flips (n : int) (p : float) : bool array =
  Array.init n (fun _ -> Rng.next () <= p)

let coin_flip_model (obs : bool array) (cap : #coin_flip_cap) : float =
  let p = cap#sample_float (Dist.beta 1. 1.) (Addr.make ()) in
  Array.iter (fun flip ->
    let _ = cap#observe_bool (Dist.bernoulli p) (Addr.make ()) flip in
    ()
  ) obs;
  p *)

(* let () =
  let true_p = 0.7 in
  let obs = Rng.handle_random (fun () -> simulate_coin_flips 50 true_p) in
  let n_heads = Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 obs in
  Printf.printf "Coin flips: %d heads out of %d\n" n_heads (Array.length obs);

  let model   = Model (coin_flip_model obs) in
  let exec    tr = exec_model_coin_flip tr model in
  let advance tr = exec_pf_coin_flip    tr model in

  (* SSMH *)
  let chain = Rng.handle_random (fun () -> ssmh 5000 exec) in
  Printf.printf "Coin SSMH: E[p] = %.4f (expected ~%.1f)\n"
    (mean (List.map (fun (p, _, _) -> p) chain)) true_p;

  (* IM *)
  let chain_im = Rng.handle_random (fun () -> im 2000 exec) in
  Printf.printf "Coin IM:   E[p] = %.4f (expected ~%.1f)\n"
    (mean (List.map (fun (p, _, _) -> p) chain_im)) true_p;

  (* MPF *)
  let fin = Rng.handle_random (fun () -> finished_values (mpf 2000 advance)) in
  Printf.printf "Coin MPF:  E[p] = %.4f (expected ~%.1f)\n"
    (mean fin) true_p;

  (* PMH *)
  let chain_pmh = Rng.handle_random (fun () -> pmh 500 150 advance exec) in
  Printf.printf "Coin PMH:  E[p] = %.4f (expected ~%.1f)\n"
    (mean (List.map (fun (p, _, _) -> p) chain_pmh)) true_p;

  (* RMPF *)
  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 1500 12 advance exec)) in
  Printf.printf "Coin RMPF: E[p] = %.4f (expected ~%.1f)\n\n"
    (mean fin_rmpf) true_p  *)

(* ── LatDiri — structural check only, output is int array ──
let () =
  let ys = simulate_lat_diri 20 in
  let exec    tr = exec_model_latdiri tr (Model (lat_diri 20 ys)) in
  let advance tr = exec_pf_latdiri    tr (Model (lat_diri 20 ys)) in

  let chain = Rng.handle_random (fun () -> ssmh 200 exec) in
  Printf.printf "LatDiri SSMH: chain length = %d (expected 200)\n" (List.length chain);

  let fin = Rng.handle_random (fun () -> finished_values (mpf 50 advance)) in
  Printf.printf "LatDiri MPF:  particles finished = %d (expected 50)\n" (List.length fin);

  let chain_pmh = Rng.handle_random (fun () -> pmh 100 10 advance) in
  Printf.printf "LatDiri PMH:  chain length = %d (expected 100)\n" (List.length chain_pmh);

  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 10 5 advance exec)) in
  Printf.printf "LatDiri RMPF: particles finished = %d (expected 10)\n\n" (List.length fin_rmpf) *)