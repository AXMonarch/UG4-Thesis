[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Pf_base
open Models

(** ADD MH STEPS TO THIS *)

let run_resample_move_pf (type a)
    (model : unit -> a)
    (num_particles : int)
    (num_mh_moves : int)
    (step_size : float)
    (resample_threshold : float) : particle_cloud =

  let cloud = pf_handler ~particles:num_particles ~resample_threshold model in
  
  if num_mh_moves > 0 then begin
    cloud
  end else
    cloud

let demo_resample_move_pf () =
  print_endline "\n=== Resample-Move PF Demo: HMM ===";
  
  let observations = [| 1.0; 2.0; 2.5; 3.0 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let num_particles = 25 in
  let num_moves = 2 in
  
  let cloud_no_moves = run_resample_move_pf hmm_model num_particles 0 0.5 0.5 in
  let cloud_with_moves = run_resample_move_pf hmm_model num_particles num_moves 0.5 0.5 in
  
  let estimate_from_cloud cloud =
    let weighted = normalize_log_weights cloud in
    List.init (Array.length observations) (fun t ->
      let state_key = "state_" ^ string_of_int t in
      let vals = Array.fold_left (fun acc (p, _w) ->
        match Hashtbl.find_opt p.trace.choices state_key with
        | Some v -> v :: acc
        | None -> acc
      ) [] weighted in
      if vals = [] then 0.0
      else List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals)
    )
  in
  
  let estimates_no_move = estimate_from_cloud cloud_no_moves in
  let estimates_with_move = estimate_from_cloud cloud_with_moves in
  
  Printf.printf "Resample-Move PF-HMM: %d particles, %d MCMC moves\n" num_particles num_moves;
  Printf.printf "Observations: [%.1f; %.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2) observations.(3);
  Printf.printf "Estimates (resample only): [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) estimates_no_move;
  Printf.printf "]\n";
  Printf.printf "Estimates (resample+move): [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) estimates_with_move;
  Printf.printf "]\n";
  print_endline "=== End Resample-Move PF Demo ===\n"
