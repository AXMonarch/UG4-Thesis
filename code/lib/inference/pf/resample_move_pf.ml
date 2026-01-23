[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects
open Pf_base
open Models


let resample_move_pf_handler
    ~(particles : int)
    ~(resample_threshold : float)
    ~(num_mh_moves : int)
    ~(step_size : float)
    ~(model_dist_tracker : (string, distribution) Hashtbl.t)
    (model : unit -> 'a)
  : particle_cloud =

  let cloud = Array.init particles (fun _ -> empty_particle ()) in

  let apply_mcmc_move particle =
    let choices_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) particle.trace.choices [] in
    if choices_list = [] then particle
    else
      let old_log_prob = particle.trace.log_prob in
      
      let idx = Random.int (List.length choices_list) in
      let (key, old_value) = List.nth choices_list idx in
      
      let proposal = old_value +. (Random.float (2.0 *. step_size) -. step_size) in
      
      match Hashtbl.find_opt model_dist_tracker key with
      | None -> particle
      | Some dist ->
          let new_log_prob = old_log_prob -. log_pdf dist old_value +. log_pdf dist proposal in
          
          let alpha = min 1.0 (exp (new_log_prob -. old_log_prob)) in
          if Random.float 1.0 < alpha then begin
            Hashtbl.replace particle.trace.choices key proposal;
            particle.trace.log_prob <- new_log_prob;
          end;
          particle
  in

  let rec handle () =
    match model () with
    | _ -> cloud

    | effect (Sample { name; dist }), k ->
        Hashtbl.replace model_dist_tracker name dist;
        Array.iter (fun p ->
          let v =
            match Hashtbl.find_opt p.trace.choices name with
            | Some v -> v
            | None ->
                let v = sample_from_dist dist in
                Hashtbl.add p.trace.choices name v;
                v
          in
          p.trace.log_prob <- p.trace.log_prob +. log_pdf dist v
        ) cloud;
        continue k (Hashtbl.find cloud.(0).trace.choices name)

    | effect (Observe { dist; obs; _ }), k ->
        Array.iter (fun p ->
          let lp = log_pdf dist obs in
          p.trace.log_prob <- p.trace.log_prob +. lp;
          p.log_weight <- p.log_weight +. lp
        ) cloud;

        let weighted = normalize_log_weights cloud in
        let ess = effective_sample_size weighted in
        let n = float_of_int particles in

        let new_cloud =
          if ess < resample_threshold *. n then begin
            let resampled = resample cloud weighted in
            Array.map (fun p ->
              let moved_p = ref p in
              for _ = 1 to num_mh_moves do
                moved_p := apply_mcmc_move !moved_p
              done;
              !moved_p
            ) resampled
          end
          else
            cloud
        in

        Array.iteri (fun i p -> cloud.(i) <- p) new_cloud;
        continue k ()
  in

  handle ()

let run_resample_move_pf (type a)
    (model : unit -> a)
    (num_particles : int)
    (num_mh_moves : int)
    (step_size : float)
    (resample_threshold : float) : particle_cloud =
  let dist_tracker = Hashtbl.create 16 in
  resample_move_pf_handler 
    ~particles:num_particles 
    ~resample_threshold 
    ~num_mh_moves 
    ~step_size 
    ~model_dist_tracker:dist_tracker
    model

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
