open Ssmh
open Linreg
open Hmm

(* Helper to compute acceptance rate *)
let compute_acceptance_rate (traces : (Effects.trace * Effects.lp_trace) list) : float =
  let rec count_unique acc prev_trace = function
    | [] -> acc
    | (trace, _) :: rest ->
        let is_different = trace.Effects.choices <> prev_trace.Effects.choices in
        count_unique (if is_different then acc + 1 else acc) trace rest
  in
  match traces with
  | [] -> 0.0
  | (first, _) :: rest ->
      let unique_count = count_unique 0 first rest in
      float_of_int unique_count /. float_of_int (List.length traces)

(* ---------- LinReg ---------- *)
let benchmark_linreg ~iters =
  Random.self_init ();
  let counter = Mtime_clock.counter () in
  let traces = ssmh ~iters linreg_model in
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  let acc_rate = compute_acceptance_rate traces in
  Printf.printf "SSMH LinReg with %4d iterations took %8.2f ms (accept: %.1f%%)\n%!" 
    iters ms (acc_rate *. 100.0)

(* ---------- HMM ---------- *)
let benchmark_hmm ~iters =
  Random.self_init ();
  let counter = Mtime_clock.counter () in
  let traces = ssmh ~iters hmm_model in
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  let acc_rate = compute_acceptance_rate traces in
  Printf.printf "SSMH HMM    with %4d iterations took %8.2f ms (accept: %.1f%%)\n%!" 
    iters ms (acc_rate *. 100.0)

let () =
  let iteration_counts =
    [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]
  in
  List.iter (fun iters ->
    benchmark_linreg ~iters;
    benchmark_hmm    ~iters;
    print_endline ""
  ) iteration_counts