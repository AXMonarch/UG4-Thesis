[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

(** Particle Metropolis-Hastings (PMH):
Note: This is a simplified version that uses standard MH.
True PMH would require a specialized handler for particle filtering
to estimate likelihoods, which goes beyond the current effect handler architecture.
This version provides the same interface but uses standard MH internally. *)

let run_particle_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (_num_particles : int)  (* Not used in simplified version *)
    (step_size : float) : a list =
  (* Use a random walk proposal like single-site MH *)
  let propose_fn _name _dist current =
    let step = (Random.float (2.0 *. step_size)) -. step_size in
    current +. step
  in
  run_mh ~iters:num_iterations ~propose:propose_fn program

let demo_particle_mh () =
  print_endline "\n=== Particle MH Demo: HMM ===";
  print_endline "Note: Simplified version using standard MH";
  
  let observations = [| 1.5; 2.0; 2.5 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let results = run_particle_mh hmm_model 40 8 0.5 in
  
  Printf.printf "Particle MH-HMM: %d iterations (simplified)\n" 40;
  Printf.printf "Observations: [%.1f; %.1f; %.1f]\n" observations.(0) observations.(1) observations.(2);
  Printf.printf "Generated %d samples\n" (List.length results);
  print_endline "=== End Particle MH Demo ===\n"

