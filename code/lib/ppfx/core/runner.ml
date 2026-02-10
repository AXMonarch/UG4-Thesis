open Effects
open Effect.Deep

(* Result of stepping a model to the next Observe *)
type 'a step_result =
  | Done of 'a * lp_trace * trace           (* Model completed *)
  | AtObserve of {
      continuation: unit -> 'a step_result;  (* Resume from this point *)
      lp_acc: lp_trace;                      (* Log-probs so far *)
      trace: trace;                          (* Trace so far *)
    }


(* Run model to completion - used by SSMH *)
let run_with_trace (model : unit -> 'a) (trace_in : trace option)
  : 'a * lp_trace * trace =
  let input_trace =
    match trace_in with
    | None -> empty_trace ()
    | Some t -> t
  in
  let out_trace = ref (empty_trace ()) in
  let lp_acc    = ref AddrMap.empty in

  (* Recursive effect handler *)
  (*A thunk is a function of type unit -> 'a 
    The reason it exists is to delay computation 
    until the effect handler is ready to handle it *)
  let rec handle thunk =
    match thunk () with
    | v -> v  (* normal return *)
    | effect (Sample { addr; dist }), k ->
        (* Look up existing value in input trace, otherwise draw *)
        let value =
          match lookup addr input_trace with
          | Some v -> v
          | None ->
              let u = Random.float 1.0 in
              Dist.draw dist u
        in
        (* Update output trace and log-prob, now storing distribution too *)
        out_trace := add_choice_with_dist addr value dist !out_trace;
        lp_acc    := AddrMap.add addr (Dist.log_prob dist value) !lp_acc;
        (* Continue the continuation and recursively handle further effects *)
        handle (fun () -> continue k value)
    | effect (Observe { addr; dist; obs }), k ->
        lp_acc := AddrMap.add addr (Dist.log_prob dist obs) !lp_acc;
        handle (fun () -> continue k ())
  in
  let result = handle model in
  (result, !lp_acc, !out_trace)

let rec advance (model : unit -> 'a) (trace_in : trace) (lp_in : lp_trace)
  : 'a step_result =
  let input_trace = trace_in in
  let out_trace = ref trace_in in
  let lp_acc = ref lp_in in
  
  let current_comp = ref model in
  let finished = ref false in
  let result = ref None in
  
  while not !finished do
    match !current_comp () with
    | v -> 
        result := Some (Done (v, !lp_acc, !out_trace));
        finished := true
    
    | effect (Sample { addr; dist }), k ->
        let value =
          match lookup addr input_trace with
          | Some v -> v
          | None ->
              let u = Random.float 1.0 in
              Dist.draw dist u
        in
        out_trace := add_choice_with_dist addr value dist !out_trace;
        lp_acc := AddrMap.add addr (Dist.log_prob dist value) !lp_acc;
        (* Update current computation to the continuation *)
        current_comp := (fun () -> continue k value)
    
    | effect (Observe { addr; dist; obs }), k ->
        (* STOP at Observe *)
        let obs_logp = Dist.log_prob dist obs in
        lp_acc := AddrMap.add addr obs_logp !lp_acc;
        
        let final_lp = !lp_acc in
        let final_trace = !out_trace in
        
        (* Return suspended computation *)
        result := Some (AtObserve {
          continuation = (fun () ->
            advance (fun () -> continue k ()) final_trace final_lp
          );
          lp_acc = final_lp;
          trace = final_trace;
        });
        finished := true
  done;
  
  match !result with
  | Some r -> r
  | None -> failwith "advance: impossible - loop ended without result"

(* Helper: Run a step_result to completion (for testing/debugging) *)
let rec run_to_completion (step : 'a step_result) : 'a * lp_trace * trace =
  match step with
  | Done (result, lp, trace) -> (result, lp, trace)
  | AtObserve { continuation; _ } ->
      run_to_completion (continuation ())

(* Helper: Check if a step_result is done *)
let is_done (step : 'a step_result) : bool =
  match step with
  | Done _ -> true
  | AtObserve _ -> false

(* Helper: Extract continuation from AtObserve (for particle stepping) *)
let get_continuation (step : 'a step_result) : (unit -> 'a step_result) option =
  match step with
  | Done _ -> None
  | AtObserve { continuation; _ } -> Some continuation

(* Helper: Extract current state from step_result *)
let get_state (step : 'a step_result) : lp_trace * trace =
  match step with
  | Done (_, lp, trace) -> (lp, trace)
  | AtObserve { lp_acc; trace; _ } -> (lp_acc, trace)