open Mh
open Pf

(* let () = Rng.load "rng_sequence.txt" *)

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
    let dx = if Rng.next () <= trans_p then 1 else 0 in
    x := !x + dx;
    ys.(i) <- Dist.draw (Rng.next ()) (Dist.binomial !x obs_p)
  done;
  ys

(* Average over n_runs to match ProbFX methodology *)
let n_runs = 10

let time_average (f : unit -> float) : float =
  let total = ref 0. in
  for _ = 1 to n_runs do
    total := !total +. f ()
  done;
  !total /. float_of_int n_runs

let time_mpf n_particles model =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = mpf n_particles model in
    let elapsed_span = Mtime_clock.count counter in
    Mtime.Span.to_float_ns elapsed_span /. 1_000_000.)

let time_pmh n_mhsteps n_particles model =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = pmh n_mhsteps n_particles model in
    let elapsed_span = Mtime_clock.count counter in
    Mtime.Span.to_float_ns elapsed_span /. 1_000_000.)

let time_rmpf n_particles n_mhsteps model =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = rmpf n_particles n_mhsteps model in
    let elapsed_span = Mtime_clock.count counter in
    Mtime.Span.to_float_ns elapsed_span /. 1_000_000.)

let time_ssmh n model =
  time_average (fun () ->
    let counter = Mtime_clock.counter () in
    let _ = ssmh n model in
    let elapsed_span = Mtime_clock.count counter in
    Mtime.Span.to_float_ns elapsed_span /. 1_000_000.)

let () =
  Random.init 42;
  let m_true = 2.0 and c_true = 1.0 in

  (* =======================================================================
     EXPERIMENT 1: Varying inference parameters with fixed model size (50)
     ======================================================================= *)

  let xs_fixed = List.init 50 (fun i -> float_of_int i /. 10.0 -. 2.5) in
  let ys_fixed = List.map (fun x ->
    m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
  ) xs_fixed in
  let ys_hmm_fixed = simulate_hmm 50 0 0.2 0.9 in

  Printf.printf "=== Experiment 1: MPF Varying Particles (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_mpf_linreg = List.map (fun n ->
    let elapsed = time_mpf n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: MPF Varying Particles (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_mpf_hmm = List.map (fun n ->
    let elapsed = time_mpf n (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (LinReg 50 obs, 50 MH steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_linreg = List.map (fun n ->
    let elapsed = time_pmh 50 n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: PMH Varying Particles (HMM 50 nodes, 50 MH steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_pmh_hmm = List.map (fun n ->
    let elapsed = time_pmh 50 n (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: RMPF Varying MH Steps (LinReg 50 obs, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "steps" "time(ms)";
  let exp1_rmpf_linreg = List.map (fun steps ->
    let elapsed = time_rmpf 10 steps (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" steps elapsed;
    (steps, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: RMPF Varying MH Steps (HMM 50 nodes, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "steps" "time(ms)";
  let exp1_rmpf_hmm = List.map (fun steps ->
    let elapsed = time_rmpf 10 steps (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" steps elapsed;
    (steps, elapsed)
  ) [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: SSMH Varying Iterations (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_ssmh_linreg = List.map (fun n ->
    let elapsed = time_ssmh n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 1: SSMH Varying Iterations (HMM 50 nodes) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let exp1_ssmh_hmm = List.map (fun n ->
    let elapsed = time_ssmh n (hmm_model 50 0 ys_hmm_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  Printf.printf "\n";

  (* =======================================================================
     EXPERIMENT 2: Varying model size with fixed inference parameters
     ======================================================================= *)

  Printf.printf "=== Experiment 2: MPF Varying Model Size (LinReg, 100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_mpf 100 (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: MPF Varying Model Size (HMM, 100 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_mpf_hmm = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_mpf 100 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size (LinReg, 50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_pmh 50 10 (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: PMH Varying Model Size (HMM, 50 steps, 10 particles) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_pmh_hmm = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_pmh 50 10 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size (LinReg, 10 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_rmpf 10 1 (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: RMPF Varying Model Size (HMM, 10 particles, 1 MH step) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_rmpf_hmm = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_rmpf 10 1 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: SSMH Varying Model Size (LinReg, 100 iterations) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_ssmh_linreg = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_ssmh 100 (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  Printf.printf "=== Experiment 2: SSMH Varying Model Size (HMM, 100 iterations) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp2_ssmh_hmm = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_ssmh 100 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  Printf.printf "\n";

  (* =======================================================================
     CSV output
     ======================================================================= *)
  let csv_file = open_out "plotting/ocaml_unified_benchmarks.csv" in

  Printf.fprintf csv_file "Num MPF particles,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_mpf_linreg));
  Printf.fprintf csv_file "MPF-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_linreg));
  Printf.fprintf csv_file "MPF-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_mpf_hmm));

  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num PMH particles,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_pmh_linreg));
  Printf.fprintf csv_file "PMH-50-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_linreg));
  Printf.fprintf csv_file "PMH-50-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_pmh_hmm));

  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num RMPF mh steps,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_rmpf_linreg));
  Printf.fprintf csv_file "RMPF-10-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_linreg));
  Printf.fprintf csv_file "RMPF-10-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_rmpf_hmm));

  Printf.fprintf csv_file "\n";

  Printf.fprintf csv_file "Num SSMH steps,%s\n"
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_ssmh_linreg));
  Printf.fprintf csv_file "SSMH-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_ssmh_linreg));
  Printf.fprintf csv_file "SSMH-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_ssmh_hmm));

  Printf.fprintf csv_file "\n";

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

  close_out csv_file;
  Printf.printf "CSV output written to plotting/ocaml_unified_benchmarks.csv\n"