[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Pf_base

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

(** Demo: Tracking with multinomial resampling *)
let demo_multinomial_pf () =
  print_endline "\n=== Multinomial PF Demo ===";
  
  let tracking_model _t () =
    let position = perform (Sample { name = "pos"; dist = Normal (5.0, 2.0) }) in
    perform (Observe { name = "obs"; dist = Normal (position, 1.0); obs = 5.5 });
    position
  in
  
  let num_particles = 20 in
  let particles = init_particles num_particles in
  let (resampled, _results) = propagate_particles (tracking_model 0) particles in
  let normalized = normalize_weights resampled in
  let after_resample = multinomial_resample normalized in
  
  let ess_before = effective_sample_size normalized in
  let ess_after = effective_sample_size after_resample in
  let est_pos = estimate_state after_resample "pos" in
  
  Printf.printf "Multinomial PF: %d particles\n" num_particles;
  Printf.printf "ESS before resample = %.2f, after = %.2f\n" ess_before ess_after;
  Printf.printf "Estimated position = %.3f (observed 5.5)\n" est_pos;
  print_endline "=== End Multinomial PF Demo ===\n"
