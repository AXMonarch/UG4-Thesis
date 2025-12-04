[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

(** Particle Metropolis-Hastings (PMH):
Uses particle filtering to estimate the likelihood,
then applies MH to sample from the posterior.
Particularly useful for sequential models. *)

type particle_state = {
  trace : trace;
  log_likelihood : float;
  num_particles : int;
}

let estimate_log_likelihood (type a) (program : unit -> a) (num_particles : int) : float =
  let log_weights = ref [] in
  
  for _i = 1 to num_particles do
    let (trace, _result) = forward_sample program in
    log_weights := trace.log_prob :: !log_weights
  done;
  
  let max_log_w = List.fold_left max neg_infinity !log_weights in
  let sum_exp = List.fold_left (fun acc lw -> acc +. exp (lw -. max_log_w)) 0.0 !log_weights in
  max_log_w +. log sum_exp -. log (float_of_int num_particles)

let pmh_kernel (type a)
    (program : unit -> a)
    (current : particle_state)
    (propose_fn : propose_fn) : particle_state * bool =
  let (proposed_trace, _) = propose_with program current.trace propose_fn in
  
  let proposed_ll = estimate_log_likelihood program current.num_particles in
  
  let alpha = acceptance_ratio current.log_likelihood proposed_ll in
  let accepted = should_accept alpha in
  
  if accepted then
    ({ trace = proposed_trace; log_likelihood = proposed_ll; num_particles = current.num_particles }, true)
  else
    (current, false)

let run_particle_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (num_particles : int)
    (propose_fn : propose_fn) : trace list * int =
  let (initial_trace, _) = forward_sample program in
  let initial_ll = estimate_log_likelihood program num_particles in
  let initial_state = { trace = initial_trace; log_likelihood = initial_ll; num_particles } in
  
  let rec iterate n current traces accepted_count =
    if n >= num_iterations then
      (List.rev traces, accepted_count)
    else
      let (next_state, was_accepted) = pmh_kernel program current propose_fn in
      let new_accepted = if was_accepted then accepted_count + 1 else accepted_count in
      iterate (n + 1) next_state (next_state.trace :: traces) new_accepted
  in
  
  iterate 0 initial_state [] 0

let demo_particle_mh () =
  print_endline "\n=== Particle MH Demo: HMM ===";
  
  let observations = [| 1.5; 2.0; 2.5 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let propose_fn _name _dist current = Some (max 0.0 (min (float_of_int num_states) (current +. (Random.float 1.0 -. 0.5)))) in
  let (traces, accepted) = run_particle_mh hmm_model 40 8 propose_fn in
  let acceptance_rate = float_of_int accepted /. 40.0 in
  let samples = List.filteri (fun i _ -> i >= 20) traces in
  let state_estimates = List.init (Array.length observations) (fun t ->
    let state_key = "state_" ^ string_of_int t in
    let vals = List.filter_map (fun trace -> Hashtbl.find_opt trace.choices state_key) samples in
    if vals = [] then 0.0 else List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals)
  ) in
  
  Printf.printf "Particle MH-HMM: %d iterations with %d particles, %.1f%% acceptance\n" 40 8 (acceptance_rate *. 100.0);
  Printf.printf "Observations: [%.1f; %.1f; %.1f]\n" observations.(0) observations.(1) observations.(2);
  Printf.printf "Estimated states: [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) state_estimates;
  Printf.printf "]\n";
  print_endline "=== End Particle MH Demo ===\n"
