open Ssmh
open Linreg

(* Run SSMH on linreg_model but do NOT print each sample *)
let run_linreg ~iters =
  Random.self_init ();
  ignore (ssmh ~iters linreg_model)  (* run SSMH, discard results *)

(* Run SSMH and report execution time in milliseconds *)
let benchmark_ssmh ~iters =
  let counter = Mtime_clock.counter () in
  run_linreg ~iters;
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  Printf.printf "SSMH with %4d iterations took %8.2f ms\n%!" iters ms

(* Example usage: increasing iteration counts *)
let () =
  let iteration_counts = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  List.iter (fun iters -> benchmark_ssmh ~iters) iteration_counts