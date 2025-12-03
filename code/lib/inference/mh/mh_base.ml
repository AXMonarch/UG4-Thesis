[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects

let empty_trace () = {
  choices = Hashtbl.create 16; (*apparently hashtable sizes in OCaml *)
  log_prob = 0.0;
}

let copy_trace t = {
  choices = Hashtbl.copy t.choices;
  log_prob = t.log_prob;
}

let sample_from_dist = function
  | Uniform (a, b) -> Utils.Stats.uniform a b
  | Normal (mu, sigma) -> 
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      let z = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
      mu +. sigma *. z

let log_pdf dist value =
  match dist with
  | Uniform (a, b) ->
      if value >= a && value <= b then
        -. log (b -. a)
      else
        neg_infinity
  | Normal (mu, sigma) ->
      let diff = value -. mu in
      -. 0.5 *. log (2.0 *. Float.pi *. sigma *. sigma) -. (diff *. diff) /. (2.0 *. sigma *. sigma)

let rec forward_sample : 'a. (unit -> 'a) -> trace * 'a = 
  fun program ->
    let trace = empty_trace () in
    let rec handle_effects : 'a. (unit -> 'a) -> 'a = fun prog ->
      match prog () with
      | result -> result
      | effect (Sample { name; dist }), k ->
          let value = sample_from_dist dist in
          let lp = log_pdf dist value in
          Hashtbl.add trace.choices name value;
          trace.log_prob <- trace.log_prob +. lp;
          continue k value
      | effect (Observe { name; dist; obs }), k ->
          let lp = log_pdf dist obs in
          trace.log_prob <- trace.log_prob +. lp;
          continue k ()
    in
    let result = handle_effects program in
    (trace, result)

let rec replay : 'a. (unit -> 'a) -> trace -> trace * 'a = 
  fun program old_trace ->
    let new_trace = empty_trace () in
    let rec handle_effects : 'a. (unit -> 'a) -> 'a = fun prog ->
      match prog () with
      | result -> result
      | effect (Sample { name; dist }), k ->
          let value = 
            match Hashtbl.find_opt old_trace.choices name with
            | Some v -> v
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
    let result = handle_effects program in
    (new_trace, result)

type propose_fn = string -> distribution -> float -> float option

let rec propose_with : 'a. (unit -> 'a) -> trace -> propose_fn -> trace * 'a = 
  fun program current_trace propose_fn ->
    let new_trace = empty_trace () in
    let rec handle_effects : 'a. (unit -> 'a) -> 'a = fun prog ->
      match prog () with
      | result -> result
      | effect (Sample { name; dist }), k ->
          let current_value = Hashtbl.find_opt current_trace.choices name in
          let value = match current_value with
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
    let result = handle_effects program in
    (new_trace, result)

let acceptance_ratio current_log_prob proposed_log_prob =
  let log_alpha = proposed_log_prob -. current_log_prob in
  min 1.0 (exp log_alpha)

let should_accept alpha =
  Random.float 1.0 < alpha

let mh_kernel (type a) 
    (program : unit -> a) 
    (current_trace : trace) 
    (propose_fn : propose_fn) : trace * bool =
  let (proposed_trace, _) = propose_with program current_trace propose_fn in
  let alpha = acceptance_ratio current_trace.log_prob proposed_trace.log_prob in
  let accepted = should_accept alpha in
  if accepted then
    (proposed_trace, true)
  else
    (current_trace, false)

let run_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (propose_fn : propose_fn) : trace list * int =
  let (initial_trace, _) = forward_sample program in
  
  let rec iterate n current traces accepted_count =
    if n >= num_iterations then
      (List.rev traces, accepted_count)
    else
      let (next_trace, was_accepted) = mh_kernel program current propose_fn in
      let new_accepted = if was_accepted then accepted_count + 1 else accepted_count in
      iterate (n + 1) next_trace (next_trace :: traces) new_accepted
  in
  
  iterate 0 initial_trace [] 0

let mh_handler : 'a. (unit -> 'a) -> propose_fn -> 'a state = 
  fun program propose_fn ->
    let handle_step trace weight prog =
      match prog () with
      | result -> (result, weight, trace)
      | effect (Propose current_trace), k ->
          let (proposed_trace, _) = propose_with program current_trace propose_fn in
          continue k proposed_trace
      | effect (Accept (current_state, proposed_state)), k ->
          let (_, w_current, t_current) = current_state in
          let (_, w_proposed, t_proposed) = proposed_state in
          let alpha = acceptance_ratio t_current.log_prob t_proposed.log_prob in
          let accepted_state = 
            if should_accept alpha then proposed_state else current_state 
          in
          continue k accepted_state
      | effect (Sample { name; dist }), k ->
          let value = sample_from_dist dist in
          let lp = log_pdf dist value in
          Hashtbl.add trace.choices name value;
          trace.log_prob <- trace.log_prob +. lp;
          continue k value
      | effect (Observe { name; dist; obs }), k ->
          let lp = log_pdf dist obs in
          trace.log_prob <- trace.log_prob +. lp;
          continue k ()
    in
    let initial_trace = empty_trace () in
    handle_step initial_trace 1.0 program

(** Toy HMM demo: Simple 2-state HMM with one observation *)
let demo_toy_hmm () =
  print_endline "\n=== Metropolis-Hastings Demo: Toy HMM ===";
  
  (* Simple probabilistic model: sample a hidden state, observe data *)
  let toy_hmm_model () =
    let state = perform (Sample { name = "state"; dist = Uniform (0.0, 2.0) }) in
    let obs_value = 1.5 in
    perform (Observe { name = "obs"; dist = Normal (state, 0.5); obs = obs_value });
    int_of_float state
  in
  
  (* Simple proposal: random walk with small step *)
  let propose_fn _name _dist current_val =
    let step = (Random.float 0.4) -. 0.2 in (* step in [-0.2, 0.2] *)
    Some (current_val +. step)
  in
  
  (* Run MH for 100 iterations *)
  let num_iterations = 100 in
  let (traces, accepted) = run_mh toy_hmm_model num_iterations propose_fn in
  
  (* Compute average state from last 50 traces (after burn-in) *)
  let burn_in = 50 in
  let samples = List.filteri (fun i _ -> i >= burn_in) traces in
  let state_samples = List.filter_map (fun trace ->
    Hashtbl.find_opt trace.choices "state"
  ) samples in
  let avg_state = 
    if state_samples = [] then 0.0
    else List.fold_left (+.) 0.0 state_samples /. float_of_int (List.length state_samples)
  in
  
  let acceptance_rate = float_of_int accepted /. float_of_int num_iterations in
  
  Printf.printf "MH: Ran %d iterations, accepted %d (%.1f%% acceptance)\n" 
    num_iterations accepted (acceptance_rate *. 100.0);
  Printf.printf "MH: Estimated hidden state = %.2f (observation was 1.5)\n" avg_state;
  Printf.printf "MH: Final trace log_prob = %.2f\n" (List.hd (List.rev traces)).log_prob;
  print_endline "=== End MH Demo ===\n"

