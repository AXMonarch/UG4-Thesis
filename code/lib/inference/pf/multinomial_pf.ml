[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Pf_base
open Models

(** Multinomial Resampling for Particle Filter:
    Standard resampling scheme that samples particles with replacement
    according to their normalized weights. *)

(** Multinomial resampling: sample N particles with replacement based on weights *)
let multinomial_resample (particles : particle_cloud) : particle_cloud =
  let n = Array.length particles in
  
  (* Normalize weights *)
  let normalized = normalize_weights particles in
  
  (* Build cumulative distribution *)
  let cumulative = Array.make n 0.0 in
  cumulative.(0) <- normalized.(0).weight;
  for i = 1 to n - 1 do
    cumulative.(i) <- cumulative.(i - 1) +. normalized.(i).weight
  done;
  
  (* Sample n particles *)
  let new_particles = Array.init n (fun _ ->
    let r = Random.float 1.0 in
    let rec find_index i =
      if i >= n - 1 || r <= cumulative.(i) then i
      else find_index (i + 1)
    in
    let idx = find_index 0 in
    copy_particle normalized.(idx)
  ) in
  
  (* Reset weights to uniform after resampling *)
  reset_weights new_particles

(** Run multinomial particle filter *)
let run_multinomial_pf (type a)
    (program : int -> unit -> a)
    (num_particles : int)
    (num_steps : int)
    (resample_threshold : float) : particle_cloud list =
  run_pf program num_particles num_steps multinomial_resample resample_threshold

(** Demo: HMM with multinomial resampling *)
let demo_multinomial_pf () =
  print_endline "\n=== Multinomial PF Demo: HMM ===";
  
  let observations = [| 1.0; 2.0; 2.5; 3.0 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let num_particles = 30 in
  let particles = init_particles num_particles in
  let (resampled, _results) = propagate_particles hmm_model particles in
  let normalized = normalize_weights resampled in
  let after_resample = multinomial_resample normalized in
  
  let ess_before = effective_sample_size normalized in
  let ess_after = effective_sample_size after_resample in
  
  let state_estimates = List.init (Array.length observations) (fun t ->
    estimate_state after_resample ("state_" ^ string_of_int t)
  ) in
  
  Printf.printf "Multinomial PF-HMM: %d particles\n" num_particles;
  Printf.printf "ESS before resample = %.2f, after = %.2f\n" ess_before ess_after;
  Printf.printf "Observations: [%.1f; %.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2) observations.(3);
  Printf.printf "Estimated states: [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) state_estimates;
  Printf.printf "]\n";
  print_endline "=== End Multinomial PF Demo ===\n"
