open Effects
open Effect.Deep
open Runner

let sum_lp (lp : lp_trace) : float =
  AddrMap.fold (fun _ v acc -> acc +. v) lp 0.0

let random_choice (xs : 'a list) : 'a =
  match xs with
  | [] -> failwith "empty list"
  | _  -> List.nth xs (Random.int (List.length xs))

(* Distribution-aware proposal function *)
let propose_for_dist (dist : distribution) (current_val : float) : float =
  match dist with
  | Normal (_mu, sigma) ->
      let step = (Random.float 2.0 -. 1.0) *. (sigma *. 0.3) in
      current_val +. step
  
  | Uniform (a, b) ->
      let range = b -. a in
      let step = (Random.float 2.0 -. 1.0) *. (range *. 0.1) in
      let proposed = current_val +. step in
      if proposed < a then a +. (a -. proposed)
      else if proposed > b then b -. (proposed -. b)
      else proposed
  
  | Beta (_, _) ->
      let step = (Random.float 2.0 -. 1.0) *. 0.1 in
      let proposed = current_val +. step in
      if proposed < 0.0 then -. proposed
      else if proposed > 1.0 then 2.0 -. proposed
      else proposed
  
  | Bernoulli _p ->
      if Random.float 1.0 < 0.2 then
        if current_val = 1.0 then 0.0 else 1.0
      else
        current_val
  
  | Binomial (n, _) ->
      let step = if Random.bool () then 1 else -1 in
      let proposed = int_of_float current_val + step in
      float_of_int (max 0 (min n proposed))
  
  | Categorical _probs ->
      if Random.float 1.0 < 0.15 then
        Dist.draw dist (Random.float 1.0)
      else
        current_val

(* Handler that threads the proposed address through using a mutable reference *)
let handle_propose_accept (f : unit -> (trace * lp_trace)) : trace * lp_trace =
  let proposed_addr_ref = ref (make_addr "init" 0) in
  let handler thunk =
    match thunk () with
    | v -> v
    | effect (Propose trace), k ->
        let addrs = addresses trace in
        let (trace', new_addr) =
          match addrs with
          | [] -> (trace, make_addr "dummy" 0)
          | _ ->
              let addr = random_choice addrs in
              let old_v = Option.get (lookup addr trace) in
              let new_trace = 
                match get_dist addr trace with
                | Some dist ->
                    let new_v = propose_for_dist dist old_v in
                    add_choice_with_dist addr new_v dist trace
                | None -> 
                    let step = 0.25 *. (Random.float 1.0 -. 0.5) *. 2.0 in
                    add_choice addr (old_v +. step) trace
              in
              (new_trace, addr)
        in
        proposed_addr_ref := new_addr;
        continue k trace'
        
    | effect (Accept ((trace, lp), (trace', lp'))), k ->
        let proposed_addr = !proposed_addr_ref in
        
        let ratio = 
          AddrMap.fold (fun addr logp_new acc ->
            match AddrMap.find_opt addr lp with
            | Some logp_old when addr <> proposed_addr ->
                acc +. (logp_new -. logp_old)
            | _ -> acc
          ) lp' 0.0
        in
        
        (* Trace size correction *)
        let trace_size = AddrMap.cardinal trace.choices in
        let trace'_size = AddrMap.cardinal trace'.choices in
        let size_correction = 
          if trace_size > 0 && trace'_size > 0 then
            log (float_of_int trace_size) -. log (float_of_int trace'_size)
          else 0.0
        in
        
        let a = min 0.0 (ratio +. size_correction) in
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