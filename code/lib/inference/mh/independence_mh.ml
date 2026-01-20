[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

(** Independence Metropolis-Hastings:
The proposal distribution is independent of the current state.
This can be more efficient when the proposal closely matches the target. *)

(*It is different to base because the proposal distribution
 does not depend on the current state 
 rather it depends only on the prior*)    

let independence_propose _name dist _current_val =
  sample_from_dist dist

let run_independence_mh (type a)
    (program : unit -> a)
    (num_iterations : int) : a list =
  run_mh ~iters:num_iterations ~propose:independence_propose program

let demo_independence_mh () =
  print_endline "\n=== Independence MH Demo: HMM ===";
  
  let observations = [| 1.5; 2.0; 2.5 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let results = run_independence_mh hmm_model 150 in
  
  Printf.printf "Independence MH-HMM: %d iterations\n" 150;
  Printf.printf "Observations: [%.1f; %.1f; %.1f]\n" observations.(0) observations.(1) observations.(2);
  Printf.printf "Generated %d samples\n" (List.length results);
  print_endline "=== End Independence MH Demo ===\n"

