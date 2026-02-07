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
        (* Update output trace and log-prob *)
        out_trace := add_choice addr value !out_trace;
        lp_acc    := AddrMap.add addr (Dist.log_prob dist value) !lp_acc;
        (* Continue the continuation and recursively handle further effects *)
        handle (fun () -> continue k value)

    | effect (Observe { addr; dist; obs }), k ->
        lp_acc := AddrMap.add addr (Dist.log_prob dist obs) !lp_acc;
        handle (fun () -> continue k ())

  in

  let result = handle model in
  (result, !lp_acc, !out_trace)