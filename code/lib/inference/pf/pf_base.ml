[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects

type particle = {
  particle_trace : trace;
  mutable weight : float;
  mutable log_weight : float;
}

(** Particle cloud is an array of particles *)
type particle_cloud = particle array

(** Create an empty particle *)
let empty_particle () = {
  particle_trace = { choices = Hashtbl.create 16; log_prob = 0.0 };
  weight = 1.0;
  log_weight = 0.0;
}

(** Sample from a distribution *)
let sample_from_dist = function
  | Uniform (a, b) -> Utils.Stats.uniform a b
  | Normal (mu, sigma) -> 
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      let z = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
      mu +. sigma *. z

(** Log probability density function *)
let log_pdf dist value =
  match dist with
  | Uniform (a, b) ->
      if value >= a && value <= b then
        -. log (b -. a)
      else
        neg_infinity
  | Normal (mu, sigma) ->
      let diff = value -. mu in
      -. 0.5 *. log (2.0 *. Float.pi *. sigma *. sigma) 
        -. (diff *. diff) /. (2.0 *. sigma *. sigma)

(** Initialize particle cloud with N particles *)
let init_particles num_particles =
  Array.init num_particles (fun _ -> empty_particle ())

(** Normalize weights so they sum to 1 *)
let normalize_weights particles =
  let total_weight = Array.fold_left (fun acc p -> acc +. p.weight) 0.0 particles in
  if total_weight > 0.0 then
    Array.map (fun p -> { p with weight = p.weight /. total_weight }) particles
  else
    particles

(** Compute effective sample size (ESS) *)
let effective_sample_size particles =
  let sum_sq = Array.fold_left (fun acc p -> acc +. (p.weight *. p.weight)) 0.0 particles in
  if sum_sq > 0.0 then 1.0 /. sum_sq else 0.0

(** Abstract resampling function type - to be implemented by variants *)
type resample_fn = particle_cloud -> particle_cloud

(** Forward sample a single particle *)
let rec forward_sample_particle : 'a. (unit -> 'a) -> particle -> particle * 'a = 
  fun program particle ->
    let rec handle_effects : 'a. (unit -> 'a) -> 'a = fun prog ->
      match prog () with
      | result -> result
      | effect (Sample { name; dist }), k ->
          let value = sample_from_dist dist in
          let lp = log_pdf dist value in
          Hashtbl.add particle.particle_trace.choices name value;
          particle.particle_trace.log_prob <- particle.particle_trace.log_prob +. lp;
          continue k value
      | effect (Observe { name; dist; obs }), k ->
          let lp = log_pdf dist obs in
          particle.particle_trace.log_prob <- particle.particle_trace.log_prob +. lp;
          particle.log_weight <- particle.log_weight +. lp;
          particle.weight <- exp particle.log_weight;
          continue k ()
    in
    let result = handle_effects program in
    (particle, result)

(** Run particle filter step - propagate all particles *)
let propagate_particles (type a) 
    (program : unit -> a) 
    (particles : particle_cloud) : particle_cloud * a list =
  let results = ref [] in
  let new_particles = Array.map (fun p ->
    let (new_p, result) = forward_sample_particle program p in
    results := result :: !results;
    new_p
  ) particles in
  (new_particles, List.rev !results)

(** Particle filter kernel - single time step *)
let pf_step (type a)
    (program : unit -> a)
    (particles : particle_cloud)
    (resample_fn : resample_fn)
    (resample_threshold : float) : particle_cloud * a list =
  (* Propagate particles forward *)
  let (propagated, results) = propagate_particles program particles in
  
  (* Normalize weights *)
  let normalized = normalize_weights propagated in
  
  (* Check if resampling is needed *)
  let ess = effective_sample_size normalized in
  let n = float_of_int (Array.length normalized) in
  
  if ess < resample_threshold *. n then
    (* Resample *)
    let resampled = resample_fn normalized in
    (resampled, results)
  else
    (normalized, results)

(** Run particle filter for sequential data *)
let run_pf (type a)
    (program : int -> unit -> a)
    (num_particles : int)
    (num_steps : int)
    (resample_fn : resample_fn)
    (resample_threshold : float) : particle_cloud list =
  let initial_particles = init_particles num_particles in
  
  let rec iterate t particles history =
    if t >= num_steps then
      List.rev (particles :: history)
    else
      let (next_particles, _) = 
        pf_step (program t) particles resample_fn resample_threshold 
      in
      iterate (t + 1) next_particles (particles :: history)
  in
  
  iterate 0 initial_particles []

(** Handler that interprets Resample effect *)
let rec pf_handler : 'a. (unit -> 'a) -> int -> resample_fn -> particle_cloud = 
  fun program num_particles resample_fn ->
    let particles = init_particles num_particles in
    let rec handle_particle_cloud () =
      match program () with
      | result -> particles
      | effect (Sample { name; dist }), k ->
          (* Sample for each particle *)
          let value = sample_from_dist dist in
          Array.iter (fun p ->
            let lp = log_pdf dist value in
            Hashtbl.add p.particle_trace.choices name value;
            p.particle_trace.log_prob <- p.particle_trace.log_prob +. lp
          ) particles;
          continue k value
      | effect (Observe { name; dist; obs }), k ->
          (* Update weights for each particle *)
          Array.iter (fun p ->
            let lp = log_pdf dist obs in
            p.particle_trace.log_prob <- p.particle_trace.log_prob +. lp;
            p.log_weight <- p.log_weight +. lp;
            p.weight <- exp p.log_weight
          ) particles;
          continue k ()
      | effect (Resample weights), k ->
          let resampled = resample_fn particles in
          Array.iteri (fun i p -> particles.(i) <- resampled.(i)) particles;
          continue k 0
    in
    handle_particle_cloud ()

(** Extract estimated state from particle cloud (weighted average) *)
let estimate_state particles var_name =
  let weighted_sum = Array.fold_left (fun acc p ->
    match Hashtbl.find_opt p.particle_trace.choices var_name with
    | Some value -> acc +. (p.weight *. value)
    | None -> acc
  ) 0.0 particles in
  weighted_sum

(** Get the best particle (highest weight) *)
let best_particle particles =
  Array.fold_left (fun best p ->
    if p.weight > best.weight then p else best
  ) particles.(0) particles

(** Sample a particle according to weights *)
let sample_particle particles =
  let r = Random.float 1.0 in
  let rec find_particle acc i =
    if i >= Array.length particles then
      particles.(Array.length particles - 1)
    else
      let acc = acc +. particles.(i).weight in
      if r <= acc then particles.(i)
      else find_particle acc (i + 1)
  in
  find_particle 0.0 0

(** Copy a particle *)
let copy_particle p = {
  particle_trace = { choices = Hashtbl.copy p.particle_trace.choices; log_prob = p.particle_trace.log_prob };
  weight = p.weight;
  log_weight = p.log_weight;
}

(** Reset weights to uniform *)
let reset_weights particles =
  let n = Array.length particles in
  let uniform_weight = 1.0 /. float_of_int n in
  Array.map (fun p -> 
    { p with weight = uniform_weight; log_weight = log uniform_weight }
  ) particles

(** Toy HMM demo: Simple 2-state HMM with one observation *)
let demo_toy_hmm () =
  print_endline "\n=== Particle Filter Demo: Toy HMM ===";
  
  (* Simple probabilistic model: sample a hidden state, observe data *)
  let toy_hmm_model () =
    let state = perform (Sample { name = "state"; dist = Uniform (0.0, 2.0) }) in
    let obs_value = 1.5 in
    perform (Observe { name = "obs"; dist = Normal (state, 0.5); obs = obs_value });
    int_of_float state
  in
  
  (* Run particle filter with 10 particles *)
  let num_particles = 10 in
  let particles = init_particles num_particles in
  
  (* Forward sample all particles *)
  let (sampled_particles, _results) = propagate_particles toy_hmm_model particles in
  let normalized = normalize_weights sampled_particles in
  
  (* Compute effective sample size *)
  let ess = effective_sample_size normalized in
  
  let estimated_state = estimate_state normalized "state" in
  
  Printf.printf "PF: Ran %d particles, ESS = %.2f\n" num_particles ess;
  Printf.printf "PF: Estimated hidden state = %.2f (observation was 1.5)\n" estimated_state;
  Printf.printf "PF: Particle weights: [";
  Array.iteri (fun i p -> 
    if i > 0 then Printf.printf "; ";
    Printf.printf "%.3f" p.weight
  ) (Array.sub normalized 0 (min 5 (Array.length normalized)));
  Printf.printf "...]\n";
  print_endline "=== End PF Demo ===\n"

