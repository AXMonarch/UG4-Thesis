[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Pf_base
open Models

let run_multinomial_pf (type a)
    (model : unit -> a)
    (num_particles : int)
    (resample_threshold : float) : particle_cloud =
  pf_handler ~particles:num_particles ~resample_threshold model

let demo_multinomial_pf () =
  print_endline "\n=== Multinomial PF Demo: HMM ===";
  
  let observations = [| 1.0; 2.0; 2.5; 3.0 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let num_particles = 30 in
  let cloud = run_multinomial_pf hmm_model num_particles 0.5 in
  
  let weighted = normalize_log_weights cloud in
  let ess = effective_sample_size weighted in
  
  let state_estimates = List.init (Array.length observations) (fun t ->
    let state_key = "state_" ^ string_of_int t in
    let vals = Array.fold_left (fun acc (p, _w) ->
      match Hashtbl.find_opt p.trace.choices state_key with
      | Some v -> v :: acc
      | None -> acc
    ) [] weighted in
      if vals = [] then 0.0
      else List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals)
  ) in
  
  Printf.printf "Multinomial PF-HMM: %d particles\n" num_particles;
  Printf.printf "ESS before resample = %.2f, after = %.2f\n" ess (float_of_int num_particles);
  Printf.printf "Observations: [%.1f; %.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2) observations.(3);
  Printf.printf "Estimated states: [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) state_estimates;
  Printf.printf "]\n";
  print_endline "=== End Multinomial PF Demo ===\n"
