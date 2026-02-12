open Effects
open Effect.Deep
open Runner

let sum_lp (lp : lp_trace) : float =
  AddrMap.fold (fun _ v acc -> acc +. v) lp 0.0

let random_choice (xs : 'a list) : 'a =
  match xs with
  | [] -> failwith "empty list"
  | _  -> List.nth xs (Random.int (List.length xs))


let handle_propose_accept (f : unit -> (trace * lp_trace))
  : trace * lp_trace =
  
  let proposed_addr_ref = ref (make_addr "init" 0) in

  let handler thunk =
    match thunk () with
    | v -> v
    | effect (Propose trace), k ->

        let addrs = addresses trace in

        let (trace', chosen_addr) =
          match addrs with
          | [] ->
              (trace, make_addr "dummy" 0)

          | _ ->
              let addr = random_choice addrs in
              let dist = Option.get (get_dist addr trace) in
              let new_val = Dist.draw dist (Random.float 1.0) in
              let new_trace =
                add_choice_with_dist addr new_val dist trace
              in
              (new_trace, addr)
        in

        proposed_addr_ref := chosen_addr;
        continue k trace'

    | effect (Accept ((trace, lp), (trace', lp'))), k ->

        let proposed_addr = !proposed_addr_ref in
        let ratio =
          AddrMap.fold
            (fun addr logp_new acc ->
              match AddrMap.find_opt addr lp with
              | Some logp_old when addr <> proposed_addr ->
                  acc +. (logp_new -. logp_old)
              | _ -> acc)
            lp'
            0.0
        in

        let size_tau =
          float_of_int (AddrMap.cardinal trace.choices)
        in
        let size_tau' =
          float_of_int (AddrMap.cardinal trace'.choices)
        in

        let size_correction =
          if size_tau > 0. && size_tau' > 0. then
            log size_tau -. log size_tau'
          else
            0.0
        in

        let log_accept = ratio +. size_correction in
        let u = log (Random.float 1.0) in

        let chosen =
          if u < log_accept
          then (trace', lp')
          else (trace, lp)
        in

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

      loop (n - 1)
        (trace_next, lp_next)
        ((trace_next, lp_next) :: acc)
  in

  loop iters (trace0, lp0) [ (trace0, lp0) ]