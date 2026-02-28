open Mh

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

let time_ssmh n model =
  let counter = Mtime_clock.counter () in
  let _ = ssmh n model in
  let elapsed_span = Mtime_clock.count counter in
  Mtime.Span.to_float_ns elapsed_span /. 1_000_000.

let () =
  Random.init 42;
  let m_true = 2.0 and c_true = 1.0 in

  Printf.printf "=== Experiment 1: Varying Iterations (LinReg 50 obs) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let xs_fixed = List.init 50 (fun i -> float_of_int i /. 10.0 -. 2.5) in
  let ys_fixed = List.map (fun x ->
    m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
  ) xs_fixed in
  let exp1_results = List.map (fun n ->
    let elapsed = time_ssmh n (lin_regr_full xs_fixed ys_fixed) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in

  Printf.printf "\n";

  Printf.printf "=== Experiment 2: Varying Model Size (100 SSMH steps) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let n_fixed = 100 in
  let exp2_results = List.map (fun size ->
    let xs = List.init size (fun i -> float_of_int i /. float_of_int size *. 4.0 -. 2.0) in
    let ys = List.map (fun x ->
      m_true *. x +. c_true +. (Random.float 2.0 -. 1.0)
    ) xs in
    let elapsed = time_ssmh n_fixed (lin_regr_full xs ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "=== Experiment 3: HMM Varying Iterations (50 steps) ===\n";
  Printf.printf "%-6s %10s\n" "n" "time(ms)";
  let ys_hmm = simulate_hmm 50 0 0.2 0.9 in
  let exp3_results = List.map (fun n ->
    let elapsed = time_ssmh n (hmm_model 50 0 ys_hmm) in
    Printf.printf "%-6d %10.2f\n" n elapsed;
    (n, elapsed)
  ) [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in

  Printf.printf "\n";
  
  Printf.printf "=== Experiment 4: HMM Varying Model Size (100 SSMH steps) ===\n";
  Printf.printf "%-6s %10s\n" "size" "time(ms)";
  let exp4_results = List.map (fun size ->
    let ys = simulate_hmm size 0 0.2 0.9 in
    let elapsed = time_ssmh 100 (hmm_model size 0 ys) in
    Printf.printf "%-6d %10.2f\n" size elapsed;
    (size, elapsed)
  ) (List.init 10 (fun i -> (i + 1) * 50)) in

  Printf.printf "\n";

  let csv_file = open_out "plotting/ocaml_ssmh_benchmarks.csv" in
  
  Printf.fprintf csv_file "Num SSMH steps,%s\n" 
    (String.concat "," (List.map (fun (n, _) -> string_of_int n) exp1_results));
  Printf.fprintf csv_file "SSMH-[ ]-LinRegr-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp1_results));
  
  Printf.fprintf csv_file "SSMH-[ ]-HidMark-50,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp3_results));
  
  Printf.fprintf csv_file "\n";
  
  Printf.fprintf csv_file "Num datapoints,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp2_results));
  Printf.fprintf csv_file "LinRegr-[ ]-SSMH-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp2_results));
  
  Printf.fprintf csv_file "\n";
  
  Printf.fprintf csv_file "Num nodes,%s\n"
    (String.concat "," (List.map (fun (s, _) -> string_of_int s) exp4_results));
  Printf.fprintf csv_file "HidMark-[ ]-SSMH-100,%s\n"
    (String.concat "," (List.map (fun (_, t) -> string_of_float (t /. 1000.)) exp4_results));
  
  close_out csv_file;
  Printf.printf "CSV output written to plotting/ocaml_ssmh_benchmarks.csv\n";
  (* ""[| x; y; z;' ... |]
  Always take the head of this list for random number  *)