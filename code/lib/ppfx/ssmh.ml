open Effects
open Effect.Deep
open Runner

let sum_lp (lp : lp_trace) : float =
  AddrMap.fold (fun _ v acc -> acc +. v) lp 0.0

let random_choice (xs : 'a list) : 'a =
  match xs with
  | [] -> failwith "empty list"
  | _  -> List.nth xs (Random.int (List.length xs))

let handle_propose_accept (f : unit -> (trace * lp_trace)) : trace * lp_trace =
  let handler thunk =
    match thunk () with
    | v -> v

    | effect (Propose trace), k ->
        let addrs = addresses trace in
        let trace' =
          match addrs with
          | [] -> trace
          | _ ->
              let addr = random_choice addrs in
              let old_v = Option.get (lookup addr trace) in
              let step = 0.25 *. (Random.float 1.0 -. 0.5) *. 2.0 in
              add_choice addr (old_v +. step) trace
        in
        continue k trace'

    | effect (Accept ((trace, lp), (trace', lp'))), k ->
        let logp  = sum_lp lp in
        let logp' = sum_lp lp' in
        let a = min 0.0 (logp' -. logp) in
        let u = log (Random.float 1.0) in
        let chosen = if u < a then (trace', lp') else (trace, lp) in
        continue k chosen
  in
  handler f

let ssmh ~(iters : int) (model : unit -> 'a)
  : (trace * lp_trace) list =
  let (_, lp0, trace0) = run_with_trace model None in

  let rec loop n (trace, lp) acc =
    if n = 0 then List.rev acc
    else
      let step () =
        let trace' = Effect.perform (Propose trace) in
        let (_, lp', trace'_used) =
          run_with_trace model (Some trace')
        in
        Effect.perform (Accept ((trace, lp), (trace'_used, lp')))
      in
      let (trace_next, lp_next) =
        handle_propose_accept step
      in
      loop (n - 1) (trace_next, lp_next) ((trace_next, lp_next) :: acc)
  in

  loop iters (trace0, lp0) [ (trace0, lp0) ]
