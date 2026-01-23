[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects
open Models

type distribution = Effects.distribution
type trace = Effects.trace
type 'a state = 'a Effects.state

let empty_trace () =
  { choices = Hashtbl.create 16; log_prob = 0.0 }

let copy_trace t =
  { choices = Hashtbl.copy t.choices; log_prob = t.log_prob }

let trace_dists = Hashtbl.create 16

let sample_from_dist = function
  | Uniform (a, b) ->
      a +. Random.float (b -. a)
  | Normal (mu, sigma) ->
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      let z =
        sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)
      in
      mu +. sigma *. z

let log_pdf dist value =
  match dist with
  | Uniform (a, b) ->
      if value >= a && value <= b
      then -.log (b -. a)
      else neg_infinity
  | Normal (mu, sigma) ->
      let diff = value -. mu in
      -.0.5 *. log (2.0 *. Float.pi *. sigma *. sigma)
      -. (diff *. diff) /. (2.0 *. sigma *. sigma)

let acceptance_ratio logp_curr logp_prop =
  let log_alpha = logp_prop -. logp_curr in
  min 1.0 (exp log_alpha)

let should_accept alpha =
  Random.float 1.0 < alpha

let mh_handler
    ~(iters : int)
    ~(propose : string -> distribution -> float -> float)
    (program : unit -> 'a)
  : 'a list =
  
  let run_with_trace init_trace =
    let trace = init_trace in
    let rec handle () =
      match program () with
      | v -> v
      | effect (Sample { name; dist }), k ->
          Hashtbl.replace trace_dists name dist;
          let value =
            match Hashtbl.find_opt trace.choices name with
            | Some v -> v
            | None ->
                let v = sample_from_dist dist in
                Hashtbl.add trace.choices name v;
                v
          in
          trace.log_prob <- trace.log_prob +. log_pdf dist value;
          continue k value
      | effect (Observe { dist; obs; _ }), k ->
          trace.log_prob <- trace.log_prob +. log_pdf dist obs;
          continue k ()
    in
    let result = handle () in
    (result, 1.0, trace)
  in

  let initial_trace = empty_trace () in
  let initial_state = run_with_trace initial_trace in

  let rec loop n (current_state : 'a state) acc =
    if n = iters then
      List.rev acc
    else
      let (_, _, curr_trace) = current_state in

      let proposed_trace =
        let t = copy_trace curr_trace in
        Hashtbl.iter (fun name v ->
          match Hashtbl.find_opt trace_dists name with
          | Some dist ->
              let new_val = propose name dist v in
              Hashtbl.replace t.choices name new_val
          | None -> ()
        ) t.choices;
        t
      in

      let proposed_state = run_with_trace proposed_trace in

      let (_, _, t_curr) = current_state in
      let (_, _, t_prop) = proposed_state in
      let alpha =
        acceptance_ratio t_curr.log_prob t_prop.log_prob
      in
      let next_state =
        if should_accept alpha then proposed_state else current_state
      in

      let (value, _, _) = next_state in
      loop (n + 1) next_state (value :: acc)
  in

  loop 0 initial_state []

let run_mh
    ~(iters : int)
    ~(propose : string -> distribution -> float -> float)
    (model : unit -> 'a)
  : 'a list =
  mh_handler ~iters ~propose model

let forward_sample (type a) (program : unit -> a) : trace * a =
  let trace = empty_trace () in
  let rec handle () =
    match program () with
    | v -> v
    | effect (Sample { name; dist }), k ->
        let value =
          match Hashtbl.find_opt trace.choices name with
          | Some v -> v
          | None ->
              let v = sample_from_dist dist in
              Hashtbl.add trace.choices name v;
              v
        in
        trace.log_prob <- trace.log_prob +. log_pdf dist value;
        continue k value
    | effect (Observe { dist; obs; _ }), k ->
        trace.log_prob <- trace.log_prob +. log_pdf dist obs;
        continue k ()
  in
  let result = handle () in
  (trace, result)

let demo_toy_hmm () =
  print_endline "\n=== Basic MH Demo: HMM ===";
  
  let observations = [| 1.2; 2.3; 1.8 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let simple_propose _name _dist current =
    current +. (Random.float 1.0 -. 0.5)
  in
  
  let results = run_mh ~iters:100 ~propose:simple_propose hmm_model in
  
  Printf.printf "Basic MH-HMM: %d iterations\n" 100;
  Printf.printf "Observations: [%.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2);
  Printf.printf "Generated %d samples\n" (List.length results);
  print_endline "=== End Basic MH Demo ===\n"
