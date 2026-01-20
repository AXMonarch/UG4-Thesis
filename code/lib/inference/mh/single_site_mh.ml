[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

let make_single_site_propose (step_size : float) =
  fun _name _dist current_val ->
    let step = (Random.float (2.0 *. step_size)) -. step_size in
    current_val +. step

let run_single_site_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (step_size : float) : a list =
  let propose_fn = make_single_site_propose step_size in
  run_mh ~iters:num_iterations ~propose:propose_fn program

let demo_single_site_mh () =
  print_endline "\n=== Single-Site MH Demo: HMM ===";
  
  let observations = [| 1.0; 2.0; 2.5; 3.0 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let results = run_single_site_mh hmm_model 250 0.5 in
  
  Printf.printf "Single-Site MH-HMM: %d iterations with step size %.1f\n" 250 0.5;
  Printf.printf "Observations: [%.1f; %.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2) observations.(3);
  Printf.printf "Generated %d samples\n" (List.length results);
  print_endline "=== End Single-Site MH Demo ===\n"

