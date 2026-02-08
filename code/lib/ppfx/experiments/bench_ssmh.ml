open Ssmh
open Linreg
open Hmm

(* ---------- LinReg ---------- *)

let run_linreg ~iters =
  Random.self_init ();
  ignore (ssmh ~iters linreg_model)

let benchmark_linreg ~iters =
  let counter = Mtime_clock.counter () in
  run_linreg ~iters;
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  Printf.printf "SSMH LinReg with %4d iterations took %8.2f ms\n%!" iters ms


(* ---------- HMM ---------- *)

let run_hmm ~iters =
  Random.self_init ();
  ignore (ssmh ~iters hmm_model)

let benchmark_hmm ~iters =
  let counter = Mtime_clock.counter () in
  run_hmm ~iters;
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  Printf.printf "SSMH HMM    with %4d iterations took %8.2f ms\n%!" iters ms

let () =
  let iteration_counts =
    [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]
  in
  List.iter (fun iters ->
    benchmark_linreg ~iters;
    benchmark_hmm    ~iters;
    print_endline ""
  ) iteration_counts