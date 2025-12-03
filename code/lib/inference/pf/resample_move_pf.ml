[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects
open Pf_base

(* Note: We need access to MH types for the move step, but since mh module
   is a sibling library, we'll define our own proposal function type here *)
type propose_fn = string -> distribution -> float -> float option

(** Resample-Move Particle Filter:
    After resampling, applies MCMC move steps to diversify particles
    and reduce particle degeneracy. Also known as particle filter with rejuvenation. *)

(** Simple MH kernel for particle rejuvenation - implements basic MH logic inline *)
let simple_mh_step_for_particle (type a)
    (program : unit -> a)
    (current_trace : trace)
    (propose_fn : propose_fn) : trace * bool =
  (* Create new trace with proposal *)
  let new_trace = { choices = Hashtbl.create 16; log_prob = 0.0 } in
  
  (* Re-run program with proposed values *)
  let rec handle_with_proposal : 'a. (unit -> 'a) -> 'a = fun prog ->
    match prog () with
    | result -> result
    | effect (Sample { name; dist }), k ->
        let current_val = Hashtbl.find_opt current_trace.choices name in
        let value = match current_val with
          | Some cv -> 
              (match propose_fn name dist cv with
               | Some proposed -> proposed
               | None -> cv)
          | None -> sample_from_dist dist
        in
        let lp = log_pdf dist value in
        Hashtbl.add new_trace.choices name value;
        new_trace.log_prob <- new_trace.log_prob +. lp;
        continue k value
    | effect (Observe { name; dist; obs }), k ->
        let lp = log_pdf dist obs in
        new_trace.log_prob <- new_trace.log_prob +. lp;
        continue k ()
  in
  
  let _ = handle_with_proposal program in
  let log_alpha = new_trace.log_prob -. current_trace.log_prob in
  let alpha = min 1.0 (exp log_alpha) in
  let accepted = Random.float 1.0 < alpha in
  
  if accepted then (new_trace, true) else (current_trace, false)

(** Apply MCMC move step to a single particle *)
let mcmc_move_particle (type a)
    (program : unit -> a)
    (particle : particle)
    (num_moves : int)
    (propose_fn : propose_fn) : particle =
  let rec apply_moves n current_trace =
    if n >= num_moves then current_trace
    else
      let (new_trace, _accepted) = simple_mh_step_for_particle program current_trace propose_fn in
      apply_moves (n + 1) new_trace
  in
  
  let final_trace = apply_moves 0 particle.particle_trace in
  { particle with particle_trace = final_trace }

(** Resample-move: resample then apply MCMC moves *)
let resample_move_step (type a)
    (program : unit -> a)
    (base_resample : resample_fn)
    (num_moves : int)
    (propose_fn : propose_fn)
    (particles : particle_cloud) : particle_cloud =
  (* First resample *)
  let resampled = base_resample particles in
  
  (* Then apply MCMC moves to each particle *)
  Array.map (fun p -> mcmc_move_particle program p num_moves propose_fn) resampled

(** Create resample-move resampling function *)
let make_resample_move (type a)
    (program : unit -> a)
    (base_resample : resample_fn)
    (num_moves : int)
    (propose_fn : propose_fn) : resample_fn =
  fun particles -> resample_move_step program base_resample num_moves propose_fn particles

(** Multinomial resampling as base resampler *)
let multinomial_resample_base (particles : particle_cloud) : particle_cloud =
  let n = Array.length particles in
  let normalized = normalize_weights particles in
  let cumulative = Array.make n 0.0 in
  cumulative.(0) <- normalized.(0).weight;
  for i = 1 to n - 1 do
    cumulative.(i) <- cumulative.(i - 1) +. normalized.(i).weight
  done;
  let new_particles = Array.init n (fun _ ->
    let r = Random.float 1.0 in
    let rec find_index i =
      if i >= n - 1 || r <= cumulative.(i) then i
      else find_index (i + 1)
    in
    let idx = find_index 0 in
    copy_particle normalized.(idx)
  ) in
  reset_weights new_particles

(** Run resample-move particle filter *)
let run_resample_move_pf (type a)
    (program : int -> unit -> a)
    (num_particles : int)
    (num_steps : int)
    (num_moves : int)
    (propose_fn : propose_fn)
    (resample_threshold : float) : particle_cloud list =
  let resample_fn = make_resample_move (program 0) multinomial_resample_base num_moves propose_fn in
  run_pf program num_particles num_steps resample_fn resample_threshold

(** Demo: Resample-move with rejuvenation *)
let demo_resample_move_pf () =
  print_endline "\n=== Resample-Move PF Demo ===";
  
  let model () =
    let state = perform (Sample { name = "state"; dist = Normal (3.0, 1.5) }) in
    perform (Observe { name = "obs"; dist = Normal (state, 0.8); obs = 4.0 });
    state
  in
  
  let propose_fn _name _dist current = Some (current +. (Random.float 0.4 -. 0.2)) in
  let num_particles = 15 in
  let particles = init_particles num_particles in
  
  (* Regular resampling *)
  let (prop1, _) = propagate_particles model particles in
  let norm1 = normalize_weights prop1 in
  let resampled_only = multinomial_resample_base norm1 in
  
  (* Resample + move *)
  let (prop2, _) = propagate_particles model particles in
  let norm2 = normalize_weights prop2 in
  let resample_fn = make_resample_move model multinomial_resample_base 3 propose_fn in
  let resampled_moved = resample_fn norm2 in
  
  let est_only = estimate_state resampled_only "state" in
  let est_moved = estimate_state resampled_moved "state" in
  
  Printf.printf "Resample-Move PF: %d particles, 3 MCMC moves\n" num_particles;
  Printf.printf "Estimate (resample only) = %.3f\n" est_only;
  Printf.printf "Estimate (resample+move) = %.3f (observed 4.0)\n" est_moved;
  print_endline "=== End Resample-Move PF Demo ===\n"
