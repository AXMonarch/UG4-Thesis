open Mhcaps

let mean lst =
  let n = float_of_int (List.length lst) in
  List.fold_left (+.) 0. lst /. n

let finished_values particles =
  List.filter_map (fun p ->
    match p.Pf.result with
    | Finished f -> Some f.value
    | Stepped  _ -> None
  ) particles

let simulate_hmm (n : int) (x0 : int) (trans_p : float) (obs_p : float) : int array =
  let x  = ref x0 in
  let ys = Array.make n 0 in
  let xs = ref [] in  (* DEBUG: track latent states *)
  for i = 0 to n - 1 do
    let dx = if Rng.next () <= trans_p then 1 else 0 in
    x := !x + dx;
    xs := !xs @ [!x];  (* DEBUG: record x *)
    ys.(i) <- Mh.Dist.draw (Rng.next ()) (Mh.Dist.binomial !x obs_p)
  done;
  Printf.printf "OCaml xs = %s\n" (String.concat " " (List.map string_of_int !xs));  (* DEBUG *)
  ys

  (* ── LinReg ── *)
let () =
  let xs = List.init 10 (fun i -> float_of_int i) in
  (* Use deterministic data: y = 3*x (matching Haskell) *)
  let ys = List.map (fun x -> 3.0 *. x) xs in
  
  (* SSMH *)
  let exec    tr = exec_model_linreg tr (Model (lin_regr_full xs ys)) in
  let chain = Rng.handle_random (fun () -> ssmh 10000 exec) in
  Printf.printf "LinReg SSMH: E[m] = %.4f (expected ~2.0)\n"
    (mean (List.map (fun ((m, _), _, _) -> m) chain));
  Printf.printf "LinReg SSMH: E[c] = %.4f (expected ~1.0)\n"
    (mean (List.map (fun ((_, c), _, _) -> c) chain));

  (* MPF *)
  let advance tr = advance_linreg tr (Model (lin_regr_full xs ys)) in
  let fin = Rng.handle_random (fun () -> finished_values (mpf 500 advance)) in
  Printf.printf "LinReg MPF:  E[m] = %.4f (expected ~2.0)\n" (mean (List.map fst fin));
  Printf.printf "LinReg MPF:  E[c] = %.4f (expected ~1.0)\n" (mean (List.map snd fin));

  (* PMH *)
  let chain_pmh = Rng.handle_random (fun () -> pmh 200 100 advance) in
  Printf.printf "LinReg PMH:  E[m] = %.4f (expected ~2.0)\n"
    (mean (List.map (fun ((m, _), _, _) -> m) chain_pmh));
  Printf.printf "LinReg PMH:  E[c] = %.4f (expected ~1.0)\n"
    (mean (List.map (fun ((_, c), _, _) -> c) chain_pmh));

  (* RMPF *)
  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 100 10 advance exec)) in
  Printf.printf "LinReg RMPF: E[m] = %.4f (expected ~2.0)\n" (mean (List.map fst fin_rmpf));
  Printf.printf "LinReg RMPF: E[c] = %.4f (expected ~1.0)\n\n" (mean (List.map snd fin_rmpf))

(* ── HMM ── *)
let () =
  (* Generate observations once with fixed seed, reuse for all algorithms *)
  let ys = Rng.handle_random (fun () -> simulate_hmm 10 0 0.2 0.9) in
  
  (* SSMH *)
  let exec tr = exec_model_hmm tr (Model (hmm_model 10 0 ys)) in
  let chain = Rng.handle_random (fun () -> ssmh 200 exec) in
  Printf.printf "HMM SSMH: E[trans_p] = %.4f (expected ~0.2)\n"
    (mean (List.map (fun ((t, _), _, _) -> t) chain));
  Printf.printf "HMM SSMH: E[obs_p]   = %.4f (expected ~0.9)\n"
    (mean (List.map (fun ((_, o), _, _) -> o) chain));

  (* MPF *)
  let advance tr = advance_hmm tr (Model (hmm_model 10 0 ys)) in
  let fin = Rng.handle_random (fun () -> finished_values (mpf 500 advance)) in
  Printf.printf "HMM MPF:  E[trans_p] = %.4f (expected ~0.2)\n" (mean (List.map fst fin));
  Printf.printf "HMM MPF:  E[obs_p]   = %.4f (expected ~0.9)\n" (mean (List.map snd fin));

  (* PMH *)
  let chain_pmh = Rng.handle_random (fun () -> pmh 200 100 advance) in
  Printf.printf "HMM PMH:  E[trans_p] = %.4f (expected ~0.2)\n"
    (mean (List.map (fun ((t, _), _, _) -> t) chain_pmh));
  Printf.printf "HMM PMH:  E[obs_p]   = %.4f (expected ~0.9)\n"
    (mean (List.map (fun ((_, o), _, _) -> o) chain_pmh));

  (* RMPF *)
  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 100 10 advance exec)) in
  Printf.printf "HMM RMPF: E[trans_p] = %.4f (expected ~0.2)\n" (mean (List.map fst fin_rmpf));
  Printf.printf "HMM RMPF: E[obs_p]   = %.4f (expected ~0.9)\n\n" (mean (List.map snd fin_rmpf))

(* ── LatDiri ── Cannot check convergence as model returns int array.
let () =
  let ys = simulate_lat_diri 20 in
  let exec    tr = exec_model_latdiri tr (Model (lat_diri 20 ys)) in
  let advance tr = advance_latdiri    tr (Model (lat_diri 20 ys)) in

  let chain = Rng.handle_random (fun () -> ssmh 200 exec) in
  Printf.printf "LatDiri SSMH: chain length = %d (expected 200)\n" (List.length chain);

  let fin = Rng.handle_random (fun () -> finished_values (mpf 50 advance)) in
  Printf.printf "LatDiri MPF:  particles finished = %d (expected 50)\n" (List.length fin);

  let chain_pmh = Rng.handle_random (fun () -> pmh 100 10 advance) in
  Printf.printf "LatDiri PMH:  chain length = %d (expected 100)\n" (List.length chain_pmh);

  let fin_rmpf = Rng.handle_random (fun () -> finished_values (rmpf 10 5 advance exec)) in
  Printf.printf "LatDiri RMPF: particles finished = %d (expected 10)\n\n" (List.length fin_rmpf) *)

  (* ── HMM Convergence Test ── *)
let () =
  let ys = Rng.handle_random (fun () -> simulate_hmm 10 0 0.2 0.9) in
  Printf.printf "OCaml ys = ";
  Array.iter (fun y -> Printf.printf "%d " y) ys;
  Printf.printf "\n%!";
  Printf.printf "%-12s  %-12s  %-12s\n" "Iterations" "E[trans_p]" "E[obs_p]";
  List.iter (fun n ->
    let (t, o) = Rng.handle_random (fun () ->
      let exec tr = exec_model_hmm tr (Model (hmm_model 10 0 ys)) in
      let chain = ssmh n exec in
      let t = mean (List.map (fun ((t, _), _, _) -> t) chain) in
      let o = mean (List.map (fun ((_, o), _, _) -> o) chain) in
      (t, o)
    ) in
    Printf.printf "%-12d  %-12.4f  %-12.4f\n" n t o
  ) [200; 1000; 5000; 5500]