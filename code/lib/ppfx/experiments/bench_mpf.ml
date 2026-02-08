open Linreg
open Mult_pf
open Hmm

(* Particle Filter for LinReg *)

let run_linreg_pf ~n_particles =
  Random.self_init ();
  ignore (mult_pf ~n_particles linreg_model)

let benchmark_linreg_pf ~n_particles =
  let counter = Mtime_clock.counter () in
  run_linreg_pf ~n_particles;
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  Printf.printf "PF LinReg with %4d particles took %8.2f ms\n%!" n_particles ms


(*  Particle Filter for HMM  *)

let run_hmm_pf ~n_particles =
  Random.self_init ();
  ignore (mult_pf ~n_particles hmm_model)

let benchmark_hmm_pf ~n_particles =
  let counter = Mtime_clock.counter () in
  run_hmm_pf ~n_particles;
  let span = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns span /. 1_000_000. in
  Printf.printf "PF HMM    with %4d particles took %8.2f ms\n%!" n_particles ms


(*  Loop over particle counts *)

let () =
  let particle_counts =
    [50;100;150;200;250;300;350;400;450;500]
  in
  List.iter (fun n ->
    benchmark_linreg_pf ~n_particles:n;
    benchmark_hmm_pf    ~n_particles:n;
    print_endline ""
  ) particle_counts