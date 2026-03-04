open Mh
open Pf

let lin_regr_full (xs : float list) (ys : float list)
    : < sample  : 'a. 'a Dist.t -> 'a;
        observe : 'b. 'b Dist.t -> 'b -> unit; .. >
    -> float * float
  = fun cap ->
    let m = cap#sample (Dist.normal 0. 3.) in
    let c = cap#sample (Dist.normal 0. 2.) in
    List.iter2 (fun x y ->
      cap#observe (Dist.normal (m *. x +. c) 1.) y
    ) xs ys;
    (m, c)

let hmm_model (n : int) (x0 : int) (ys : int array)
    : < sample  : 'a. 'a Dist.t -> 'a;
        observe : 'b. 'b Dist.t -> 'b -> unit; .. >
    -> float * float
  = fun cap ->
    let trans_p = cap#sample (Dist.beta 2. 2.) in
    let obs_p   = cap#sample (Dist.beta 2. 2.) in
    let x       = ref x0 in
    for i = 0 to n - 1 do
      let dx = if cap#sample (Dist.bernoulli trans_p) then 1 else 0 in
      x := !x + dx;
      cap#observe (Dist.binomial !x obs_p) ys.(i)
    done;
    (trans_p, obs_p)

let simulate_hmm (n : int) (x0 : int) (trans_p : float) (obs_p : float) : int array =
  let x  = ref x0 in
  let ys = Array.make n 0 in
  for i = 0 to n - 1 do
    let dx = if Random.float 1.0 <= trans_p then 1 else 0 in
    x := !x + dx;
    ys.(i) <- Dist.draw (Random.float 1.0) (Dist.binomial !x obs_p)
  done;
  ys

let time_mpf n_particles model =
  let counter = Mtime_clock.counter () in
  let _ = mpf n_particles model in
  let elapsed_span = Mtime_clock.count counter in
  Mtime.Span.to_float_ns elapsed_span /. 1_000_000.

let time_pmh n_mhsteps n_particles model =
  let counter = Mtime_clock.counter () in
  let _ = pmh n_mhsteps n_particles model in
  let elapsed_span = Mtime_clock.count counter in
  Mtime.Span.to_float_ns elapsed_span /. 1_000_000.

let time_rmpf n_particles n_mhsteps model =
  let counter = Mtime_clock.counter () in
  let _ = rmpf n_particles n_mhsteps model in
  let elapsed_span = Mtime_clock.count counter in
  Mtime.Span.to_float_ns elapsed_span /. 1_000_000.

let () =
  Random.init 42;
  let m_true = 2.0 and c_true = 1.0 in

  (* --- Experiment 1: Varying particles, fixed model (LinReg 50 obs) --- *)
  Printf.printf "=== Experiment 1: MPF Varying Particles (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let xs_fixed = List.init 50 (fun i -> float_of_int i /. 10.0 -. 2.5) in
  let ys_fixed = List.map (fun x ->
    m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
  ) xs_fixed in
  let exp1_mpf_results = List.map (fun n ->
    let elapsed = time_mpf n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 1: RMPF Varying Particles (LinReg 50 obs, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let n_mhsteps_rmpf = 1 in
  let exp1_rmpf_results = List.map (fun n ->
    let elapsed = time_rmpf n n_mhsteps_rmpf (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let n_pmh_fixed = 50 in
  let exp1_pmh_results = List.map (fun n ->
    let elapsed = time_pmh n_pmh_fixed n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 10)) in

  Printf.printf "\n";

  (* --- Experiment 1: HMM varying particles --- *)
  Printf.printf "=== Experiment 1: MPF Varying Particles (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let ys_hmm_fixed = simulate_hmm 50 0 0.2 0.9 in
  let exp1_mpf_hmm_results = List.map (fun n ->
    let elapsed = time_mpf n (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 1: RMPF Varying Particles (HMM 50 nodes, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_rmpf_hmm_results = List.map (fun n ->
    let elapsed = time_rmpf n n_mhsteps_rmpf (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_hmm_results = List.map (fun n ->
    let elapsed = time_pmh n_pmh_fixed n (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 10)) in

  Printf.printf "\n";

  (* --- Experiment 2: Varying model size, fixed particles --- *)
  Printf.printf "=== Experiment 2: MPF Varying Model Size (100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let n_mpf_fixed = 100 in
  let exp2_mpf_results = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_mpf n_mpf_fixed (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size (100 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_results = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_rmpf n_mpf_fixed n_mhsteps_rmpf (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size (50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_results = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_pmh n_pmh_fixed 10 (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  (* --- Experiment 2: HMM varying model size --- *)
  Printf.printf "=== Experiment 2: MPF Varying Model Size HMM (100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_hmm_results = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_mpf n_mpf_fixed (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size HMM (100 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_hmm_results = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_rmpf n_mpf_fixed n_mhsteps_rmpf (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size HMM (50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_hmm_results = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_pmh n_pmh_fixed 10 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  (* --- CSV output --- *)
  let csv_file = open_out "plotting/ocaml_pf_benchmarks.csv" in

  Printf.fprintf csv_file "Num particles,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_mpf_results));
  Printf.fprintf csv_file "MPF-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_results));
  Printf.fprintf csv_file "RMPF-1-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_results));
  Printf.fprintf csv_file "PMH-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_results));
  Printf.fprintf csv_file "MPF-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_hmm_results));
  Printf.fprintf csv_file "RMPF-1-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_hmm_results));
  Printf.fprintf csv_file "PMH-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_hmm_results));

  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num datapoints,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_mpf_results));
  Printf.fprintf csv_file "LinRegr-[ ]-MPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_mpf_results));
  Printf.fprintf csv_file "LinRegr-1-RMPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_rmpf_results));
  Printf.fprintf csv_file "LinRegr-[ ]-PMH-50-10,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_pmh_results));

  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num nodes,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_mpf_hmm_results));
  Printf.fprintf csv_file "HidMark-[ ]-MPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_mpf_hmm_results));
  Printf.fprintf csv_file "HidMark-1-RMPF-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_rmpf_hmm_results));
  Printf.fprintf csv_file "HidMark-[ ]-PMH-50-10,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_pmh_hmm_results));

  close_out csv_file;
  Printf.printf "CSV output written to plotting/ocaml_pf_benchmarks.csv\n"