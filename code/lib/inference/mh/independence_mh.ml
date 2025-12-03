[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base

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
  print_endline "\n=== Independence MH Demo ===";
  
  let coin_model () =
    let bias = perform (Sample { name = "bias"; dist = Uniform (0.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (bias, 0.1); obs = 0.7 });
    bias
  in
  
  let (traces, accepted) = run_independence_mh coin_model 200 in
  let acceptance_rate = float_of_int accepted /. 200.0 in
  let samples = List.filteri (fun i _ -> i >= 100) traces in
  let bias_samples = List.filter_map (fun t -> Hashtbl.find_opt t.choices "bias") samples in
  let avg_bias = List.fold_left (+.) 0.0 bias_samples /. float_of_int (List.length bias_samples) in
  
  Printf.printf "Independence MH: %d iterations, %.1f%% acceptance\n" 200 (acceptance_rate *. 100.0);
  Printf.printf "Estimated coin bias = %.3f (observed 0.7)\n" avg_bias;
  print_endline "=== End Independence MH Demo ===\n"
