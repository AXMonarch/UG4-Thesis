open Effect
open Effect.Deep
open Effects

(** A trace records all the random choices made during execution *)
type trace = {
  choices : (string, float) Hashtbl.t;  (* Map from variable name to value *)
  log_prob : float;                      (* Log probability of this trace *)
}

(** Create an empty trace *)
let empty_trace () = {
  choices = Hashtbl.create 16;
  log_prob = 0.0;
}

(** Copy a trace *)
let copy_trace t = {
  choices = Hashtbl.copy t.choices;
  log_prob = t.log_prob;
}

(** Sample from a distribution *)
let sample_from_dist = function
  | Uniform (a, b) -> Stats.uniform a b
  | Normal (mu, sigma) -> mu +. (Random.float 2.0 -. 1.0) *. sigma *. 1.73205  (* Simplified normal *)

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
      -. 0.5 *. log (2.0 *. Float.pi *. sigma *. sigma) -. (diff *. diff) /. (2.0 *. sigma *. sigma)

(** Propose a new value for a variable *)
let propose_value dist current =
  match dist with
  | Uniform (a, b) -> Stats.uniform a b
  | Normal (mu, sigma) ->
      current +. (Random.float 2.0 -. 1.0) *. sigma *. 0.5

(** MH state contains current and proposed traces *)
type mh_state = {
  current_trace : trace;
  proposed_trace : trace;
  mutable building_proposal : bool;
}

(** Create initial MH state *)
let init_state () = {
  current_trace = empty_trace ();
  proposed_trace = empty_trace ();
  building_proposal = false;
}

(** Forward sampling handler - builds a trace from scratch *)
let forward_sample (type a) (program : unit -> a) : trace * a =
  let trace = empty_trace () in
  let result = match program () with
    | value -> (trace, value)
    | effect (Sample { name; dist }) k ->
        let value = sample_from_dist dist in
        let lp = log_pdf dist value in
        Hashtbl.add trace.choices name value;
        trace.log_prob <- trace.log_prob +. lp;
        continue k value
    | effect (Observe { name; dist; obs }) k ->
        let lp = log_pdf dist obs in
        trace.log_prob <- trace.log_prob +. lp;
        continue k ()
  in
  result

(** Replay handler - re-executes using values from a trace *)
let replay (type a) (program : unit -> a) (trace : trace) : trace * a =
  let new_trace = empty_trace () in
  let result = match program () with
    | value -> (new_trace, value)
    | effect (Sample { name; dist }) k ->
        let value = 
          match Hashtbl.find_opt trace.choices name with
          | Some v -> v
          | None -> sample_from_dist dist
        in
        let lp = log_pdf dist value in
        Hashtbl.add new_trace.choices name value;
        new_trace.log_prob <- new_trace.log_prob +. lp;
        continue k value
    | effect (Observe { name; dist; obs }) k ->
        let lp = log_pdf dist obs in
        new_trace.log_prob <- new_trace.log_prob +. lp;
        continue k ()
  in
  result

(** Propose a new trace by modifying some variables *)
let propose_trace (type a) (program : unit -> a) (current : trace) (propose_fn : string -> distribution -> float -> float option) : trace * a =
  let new_trace = empty_trace () in
  let result = match program () with
    | value -> (new_trace, value)
    | effect (Sample { name; dist }) k ->
        let current_value = Hashtbl.find_opt current.choices name in
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
    | effect (Observe { name; dist; obs }) k ->
        let lp = log_pdf dist obs in
        new_trace.log_prob <- new_trace.log_prob +. lp;
        continue k ()
  in
  result

(** Metropolis-Hastings acceptance probability *)
let acceptance_ratio current_log_prob proposed_log_prob =
  let log_alpha = proposed_log_prob -. current_log_prob in
  min 1.0 (exp log_alpha)

(** Accept/reject decision *)
let accept alpha =
  Random.float 1.0 < alpha

(** Run a single MH step *)
let mh_step (type a) 
    (program : unit -> a) 
    (current : trace) 
    (propose_fn : string -> distribution -> float -> float option) : trace * bool =
  let (proposed, _) = propose_trace program current propose_fn in
  let alpha = acceptance_ratio current.log_prob proposed.log_prob in
  let accepted = accept alpha in
  if accepted then
    (proposed, true)
  else
    (current, false)

(** Run MH for multiple iterations *)
let run_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (propose_fn : string -> distribution -> float -> float option) : trace list * int =
  (* Initialize with forward sample *)
  let (initial_trace, _) = forward_sample program in
  
  let rec iterate n current traces accepted_count =
    if n >= num_iterations then
      (List.rev traces, accepted_count)
    else
      let (next_trace, was_accepted) = mh_step program current propose_fn in
      let new_accepted = if was_accepted then accepted_count + 1 else accepted_count in
      iterate (n + 1) next_trace (next_trace :: traces) new_accepted
  in
  
  iterate 0 initial_trace [] 0

(** Extract a specific variable from a trace *)
let get_variable trace name =
  Hashtbl.find_opt trace.choices name

(** Get all variables from a trace as an association list *)
let trace_to_list trace =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) trace.choices []

(** Print trace information *)
let print_trace trace =
  Printf.printf "Trace (log_prob = %.4f):\n" trace.log_prob;
  Hashtbl.iter (fun name value ->
    Printf.printf "  %s = %.4f\n" name value
  ) trace.choices

(** Compute acceptance rate *)
let acceptance_rate accepted total =
  float_of_int accepted /. float_of_int total
