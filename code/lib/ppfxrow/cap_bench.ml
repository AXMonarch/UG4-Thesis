open Mhcaps

(* let () = Rng.load "rng_sequence.txt" *)

let m_true = 2.0 and c_true = 1.0

let n_runs = 10

let time_average f =
  let total = ref 0. in
  for _ = 1 to n_runs do
    total := !total +. f ()
  done;
  !total /. float_of_int n_runs

let time_ssmh n exec =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = Rng.handle_random (fun () -> ssmh n exec) in
    Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1_000_000.)

let time_mpf n_particles advance =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = Rng.handle_random (fun () -> mpf n_particles advance) in
    Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1_000_000.)

let time_pmh n_mhsteps n_particles advance =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = Rng.handle_random (fun () -> pmh n_mhsteps n_particles advance) in
    Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1_000_000.)

let time_rmpf n_particles n_mhsteps advance exec =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = Rng.handle_random (fun () -> rmpf n_particles n_mhsteps advance exec) in
    Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1_000_000.)

let simulate_hmm (n : int) (x0 : int) (trans_p : float) (obs_p : float) : int array =
  let x  = ref x0 in
  let ys = Array.make n 0 in
  for i = 0 to n - 1 do
    let dx = if Rng.next () <= trans_p then 1 else 0 in
    x := !x + dx;
    ys.(i) <- Mh.Dist.draw (Rng.next ()) (Mh.Dist.binomial !x obs_p)
  done;
  ys

let () =
  (* =======================================================================
     EXPERIMENT 1: Varying inference parameters, fixed model size (50)
     ======================================================================= *)

  let xs_fixed = List.init 50 (fun i -> float_of_int i /. 10.0 -. 2.5) in
  let ys_fixed = Rng.handle_random (fun () ->
    List.map (fun x ->
      m_true *. x +. c_true +. (Rng.next () *. 2.0 -. 1.0)
    ) xs_fixed
  ) in
  let ys_hmm_fixed  = Rng.handle_random (fun () -> simulate_hmm 50 0 0.2 0.9) in
  let ys_lda_fixed  = simulate_lat_diri 50 in

  (* --- SSMH varying iterations --- *)
  Printf.printf "=== Experiment 1: SSMH Varying Iterations (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_ssmh_linreg = List.map (fun n ->
    let exec tr = exec_model_linreg tr (Model (lin_regr_full xs_fixed ys_fixed)) in
    let elapsed = time_ssmh n exec in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "SSMH LinReg Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: SSMH Varying Iterations (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_ssmh_hmm = List.map (fun n ->
    let exec tr = exec_model_hmm tr (Model (hmm_model 50 0 ys_hmm_fixed)) in
    let elapsed = time_ssmh n exec in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "SSMH HMM Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: SSMH Varying Iterations (LatDiri 50 words) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_ssmh_latdiri = List.map (fun n ->
    let exec tr = exec_model_latdiri tr (Model (lat_diri 50 ys_lda_fixed)) in
    let elapsed = time_ssmh n exec in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "SSMH LatDiri Experiment 1 complete\n\n";

  (* --- MPF varying particles --- *)
  Printf.printf "=== Experiment 1: MPF Varying Particles (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_mpf_linreg = List.map (fun n ->
    let advance tr = advance_linreg tr (Model (lin_regr_full xs_fixed ys_fixed)) in
    let elapsed = time_mpf n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "MPF LinReg Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: MPF Varying Particles (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_mpf_hmm = List.map (fun n ->
    let advance tr = advance_hmm tr (Model (hmm_model 50 0 ys_hmm_fixed)) in
    let elapsed = time_mpf n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "MPF HMM Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: MPF Varying Particles (LatDiri 50 words) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_mpf_latdiri = List.map (fun n ->
    let advance tr = advance_latdiri tr (Model (lat_diri 50 ys_lda_fixed)) in
    let elapsed = time_mpf n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "MPF LatDiri Experiment 1 complete\n\n";

  (* --- PMH varying particles --- *)
  Printf.printf "=== Experiment 1: PMH Varying Particles (LinReg 50 obs, 50 MH steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_linreg = List.map (fun n ->
    let advance tr = advance_linreg tr (Model (lin_regr_full xs_fixed ys_fixed)) in
    let elapsed = time_pmh 50 n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "PMH LinReg Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (HMM 50 nodes, 50 MH steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_hmm = List.map (fun n ->
    let advance tr = advance_hmm tr (Model (hmm_model 50 0 ys_hmm_fixed)) in
    let elapsed = time_pmh 50 n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "PMH HMM Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (LatDiri 50 words, 50 MH steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_latdiri = List.map (fun n ->
    let advance tr = advance_latdiri tr (Model (lat_diri 50 ys_lda_fixed)) in
    let elapsed = time_pmh 50 n advance in
    Printf.printf "%-6d %10.2f\n" n elapsed; (n, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "PMH LatDiri Experiment 1 complete\n\n";

  (* --- RMPF varying MH steps --- *)
  Printf.printf "=== Experiment 1: RMPF Varying MH Steps (LinReg 50 obs, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "steps" "time(ms)";
  let exp1_rmpf_linreg = List.map (fun steps ->
    let advance tr = advance_linreg tr (Model (lin_regr_full xs_fixed ys_fixed)) in
    let exec    tr = exec_model_linreg tr (Model (lin_regr_full xs_fixed ys_fixed)) in
    let elapsed = time_rmpf 10 steps advance exec in
    Printf.printf "%-6d %10.2f\n" steps elapsed; (steps, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "RMPF LinReg Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: RMPF Varying MH Steps (HMM 50 nodes, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "steps" "time(ms)";
  let exp1_rmpf_hmm = List.map (fun steps ->
    let advance tr = advance_hmm tr (Model (hmm_model 50 0 ys_hmm_fixed)) in
    let exec    tr = exec_model_hmm tr (Model (hmm_model 50 0 ys_hmm_fixed)) in
    let elapsed = time_rmpf 10 steps advance exec in
    Printf.printf "%-6d %10.2f\n" steps elapsed; (steps, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "RMPF HMM Experiment 1 complete\n\n";

  Printf.printf "=== Experiment 1: RMPF Varying MH Steps (LatDiri 50 words, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "steps" "time(ms)";
  let exp1_rmpf_latdiri = List.map (fun steps ->
    let advance tr = advance_latdiri tr (Model (lat_diri 50 ys_lda_fixed)) in
    let exec    tr = exec_model_latdiri tr (Model (lat_diri 50 ys_lda_fixed)) in
    let elapsed = time_rmpf 10 steps advance exec in
    Printf.printf "%-6d %10.2f\n" steps elapsed; (steps, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";
  Printf.printf "RMPF LatDiri Experiment 1 complete\n\n";

  (* =======================================================================
     EXPERIMENT 2: Varying model size, fixed inference parameters
     ======================================================================= *)

  (* --- SSMH varying model size --- *)
  Printf.printf "=== Experiment 2: SSMH Varying Model Size (LinReg, 100 iterations) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_ssmh_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = Rng.handle_random (fun () -> List.map (fun x -> m_true *. x +. c_true +. (Rng.next() *. 2.0 -. 1.0)) xs) in
    let exec tr = exec_model_linreg tr (Model (lin_regr_full xs ys)) in
    let elapsed = time_ssmh 100 exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "SSMH LinReg Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: SSMH Varying Model Size (HMM, 100 iterations) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_ssmh_hmm = List.map (fun size ->
    let ys = Rng.handle_random (fun () -> simulate_hmm size 0 0.2 0.9) in
    let exec tr = exec_model_hmm tr (Model (hmm_model size 0 ys)) in
    let elapsed = time_ssmh 100 exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "SSMH HMM Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: SSMH Varying Model Size (LatDiri, 100 iterations) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_ssmh_latdiri = List.map (fun size ->
    let ys = simulate_lat_diri size in
    let exec tr = exec_model_latdiri tr (Model (lat_diri size ys)) in
    let elapsed = time_ssmh 100 exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "SSMH LatDiri Experiment 2 complete\n\n";

  (* --- MPF varying model size --- *)
  Printf.printf "=== Experiment 2: MPF Varying Model Size (LinReg, 100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = Rng.handle_random (fun () -> List.map (fun x -> m_true *. x +. c_true +. (Rng.next() *. 2.0 -. 1.0)) xs) in
    let advance tr = advance_linreg tr (Model (lin_regr_full xs ys)) in
    let elapsed = time_mpf 100 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "MPF LinReg Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: MPF Varying Model Size (HMM, 100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_hmm = List.map (fun size ->
    let ys = Rng.handle_random (fun () -> simulate_hmm size 0 0.2 0.9) in
    let advance tr = advance_hmm tr (Model (hmm_model size 0 ys)) in
    let elapsed = time_mpf 100 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "MPF HMM Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: MPF Varying Model Size (LatDiri, 100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_latdiri = List.map (fun size ->
    let ys = simulate_lat_diri size in
    let advance tr = advance_latdiri tr (Model (lat_diri size ys)) in
    let elapsed = time_mpf 100 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "MPF LatDiri Experiment 2 complete\n\n";

  (* --- PMH varying model size --- *)
  Printf.printf "=== Experiment 2: PMH Varying Model Size (LinReg, 50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = Rng.handle_random (fun () -> List.map (fun x -> m_true *. x +. c_true +. (Rng.next() *. 2.0 -. 1.0)) xs) in
    let advance tr = advance_linreg tr (Model (lin_regr_full xs ys)) in
    let elapsed = time_pmh 50 10 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "PMH LinReg Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size (HMM, 50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_hmm = List.map (fun size ->
    let ys = Rng.handle_random (fun () -> simulate_hmm size 0 0.2 0.9) in
    let advance tr = advance_hmm tr (Model (hmm_model size 0 ys)) in
    let elapsed = time_pmh 50 10 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "PMH HMM Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size (LatDiri, 50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_latdiri = List.map (fun size ->
    let ys = simulate_lat_diri size in
    let advance tr = advance_latdiri tr (Model (lat_diri size ys)) in
    let elapsed = time_pmh 50 10 advance in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "PMH LatDiri Experiment 2 complete\n\n";

  (* --- RMPF varying model size --- *)
  Printf.printf "=== Experiment 2: RMPF Varying Model Size (LinReg, 10 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = Rng.handle_random (fun () -> List.map (fun x -> m_true *. x +. c_true +. (Rng.next() *. 2.0 -. 1.0)) xs) in
    let advance tr = advance_linreg tr (Model (lin_regr_full xs ys)) in
    let exec    tr = exec_model_linreg tr (Model (lin_regr_full xs ys)) in
    let elapsed = time_rmpf 10 1 advance exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "RMPF LinReg Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size (HMM, 10 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_hmm = List.map (fun size ->
    let ys = Rng.handle_random (fun () -> simulate_hmm size 0 0.2 0.9) in
    let advance tr = advance_hmm tr (Model (hmm_model size 0 ys)) in
    let exec    tr = exec_model_hmm tr (Model (hmm_model size 0 ys)) in
    let elapsed = time_rmpf 10 1 advance exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";
  Printf.printf "RMPF HMM Experiment 2 complete\n\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size (LatDiri, 10 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_latdiri = List.map (fun size ->
    let ys = simulate_lat_diri size in
    let advance tr = advance_latdiri tr (Model (lat_diri size ys)) in
    let exec    tr = exec_model_latdiri tr (Model (lat_diri size ys)) in
    let elapsed = time_rmpf 10 1 advance exec in
    Printf.printf "%-6d %10.2f\n" size elapsed; (size, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";
  Printf.printf "RMPF LatDiri Experiment 2 complete\n\n";

  (* =======================================================================
     CSV output
     ======================================================================= *)
  let csv_file = open_out "plotting/ocaml_cap_benchmarks.csv" in

  (* Experiment 1 *)
  Printf.fprintf csv_file "Num SSMH steps,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_ssmh_linreg));
  Printf.fprintf csv_file "SSMH-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_ssmh_linreg));
  Printf.fprintf csv_file "SSMH-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_ssmh_hmm));
  Printf.fprintf csv_file "SSMH-[ ]-LatDiri-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_ssmh_latdiri));
  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num MPF particles,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_mpf_linreg));
  Printf.fprintf csv_file "MPF-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_linreg));
  Printf.fprintf csv_file "MPF-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_hmm));
  Printf.fprintf csv_file "MPF-[ ]-LatDiri-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_latdiri));
  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num PMH particles,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_pmh_linreg));
  Printf.fprintf csv_file "PMH-50-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_linreg));
  Printf.fprintf csv_file "PMH-50-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_hmm));
  Printf.fprintf csv_file "PMH-50-[ ]-LatDiri-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_latdiri));
  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num RMPF mh steps,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_rmpf_linreg));
  Printf.fprintf csv_file "RMPF-10-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_linreg));
  Printf.fprintf csv_file "RMPF-10-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_hmm));
  Printf.fprintf csv_file "RMPF-10-[ ]-LatDiri-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_latdiri));
  Printf.fprintf csv_file "\n";

  (* Experiment 2 *)
  Printf.fprintf csv_file "Num datapoints,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_mpf_linreg));
  Printf.fprintf csv_file "LinRegr-[ ]-MPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_mpf_linreg));
  Printf.fprintf csv_file "LinRegr-[ ]-PMH-50-10,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_pmh_linreg));
  Printf.fprintf csv_file "LinRegr-[ ]-RMPF-10-1,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_rmpf_linreg));
  Printf.fprintf csv_file "LinRegr-[ ]-SSMH-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_ssmh_linreg));
  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num nodes,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_mpf_hmm));
  Printf.fprintf csv_file "HidMark-[ ]-MPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_mpf_hmm));
  Printf.fprintf csv_file "HidMark-[ ]-PMH-50-10,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_pmh_hmm));
  Printf.fprintf csv_file "HidMark-[ ]-RMPF-10-1,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_rmpf_hmm));
  Printf.fprintf csv_file "HidMark-[ ]-SSMH-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_ssmh_hmm));
  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num words,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_mpf_latdiri));
  Printf.fprintf csv_file "LatDiri-[ ]-MPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_mpf_latdiri));
  Printf.fprintf csv_file "LatDiri-[ ]-PMH-50-10,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_pmh_latdiri));
  Printf.fprintf csv_file "LatDiri-[ ]-RMPF-10-1,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_rmpf_latdiri));
  Printf.fprintf csv_file "LatDiri-[ ]-SSMH-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_ssmh_latdiri));

  close_out csv_file;
  Printf.printf "CSV output written to plotting/ocaml_cap_benchmarks.csv\n"