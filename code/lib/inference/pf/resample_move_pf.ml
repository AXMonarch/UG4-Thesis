[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Pf_base
open Models


let apply_ssmh_moves (cloud : particle_cloud) (num_moves : int) (step_size : float) : particle_cloud =
  if num_moves = 0 then cloud
  else
    Array.map (fun particle ->
      let trace = particle.trace in
      let choices_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) trace.choices [] in
      
      if choices_list = [] then particle
      else begin
        for _ = 1 to num_moves do
          let idx = Random.int (List.length choices_list) in
          let (key, old_value) = List.nth choices_list idx in
          
          let proposal = old_value +. (Random.float (2.0 *. step_size) -. step_size) in
          
          Hashtbl.replace trace.choices key proposal
        done;
        particle
      end
    ) cloud

let run_resample_move_pf (type a)
    (model : unit -> a)
    (num_particles : int)
    (num_mh_moves : int)
    (step_size : float)
    (resample_threshold : float) : particle_cloud =

  let cloud = pf_handler ~particles:num_particles ~resample_threshold model in
  
  (* Apply SSMH moves to particles after resampling *)
  apply_ssmh_moves cloud num_mh_moves step_size

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
