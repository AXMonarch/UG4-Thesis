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
  Some (sample_from_dist dist)

let run_independence_mh (type a)
    (program : unit -> a)
    (num_iterations : int) : trace list * int =
  run_mh program num_iterations independence_propose

let demo_independence_mh () =
  print_endline "\n=== Independence MH Demo: HMM ===";
  
  let observations = [| 1.5; 2.0; 2.5 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let (traces, accepted) = run_independence_mh hmm_model 150 in
  let acceptance_rate = float_of_int accepted /. 150.0 in
  let samples = List.filteri (fun i _ -> i >= 75) traces in
  let state_estimates = List.init (Array.length observations) (fun t ->
    let state_key = "state_" ^ string_of_int t in
    let vals = List.filter_map (fun trace -> Hashtbl.find_opt trace.choices state_key) samples in
    if vals = [] then 0.0 else List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals)
  ) in
  
  Printf.printf "Independence MH-HMM: %d iterations, %.1f%% acceptance\n" 150 (acceptance_rate *. 100.0);
  Printf.printf "Observations: [%.1f; %.1f; %.1f]\n" observations.(0) observations.(1) observations.(2);
  Printf.printf "Estimated states: [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) state_estimates;
  Printf.printf "]\n";
  print_endline "=== End Independence MH Demo ===\n"
