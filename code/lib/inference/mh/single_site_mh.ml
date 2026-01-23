[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

let run_single_site_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (step_size : float) : a list =
  let selected_var = ref "" in
  let propose_fn name _dist current_val =
    if !selected_var = "" then selected_var := name;
    if name = !selected_var then
      let step = (Random.float (2.0 *. step_size)) -. step_size in
      current_val +. step
    else
      current_val
  in
  let all_results = ref [] in
  for _ = 1 to num_iterations do
    selected_var := "";
    let results = run_mh ~iters:1 ~propose:propose_fn program in
    all_results := results @ !all_results
  done;
  !all_results

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

