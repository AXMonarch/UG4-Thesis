open Effects
open Effect.Deep

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