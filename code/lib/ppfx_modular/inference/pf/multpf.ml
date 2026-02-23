open Effects
open Types
open Model_handlers
open Advance
open Pf

let normalise (log_ws : float list) : float list =
  let max_lw  = List.fold_left max neg_infinity log_ws in
  let shifted = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
  let total   = List.fold_left ( +. ) 0.0 shifted in
  List.map (fun s -> s /. total) shifted

let log_mean_exp (log_ws : float list) : float =
  let n      = float_of_int (List.length log_ws) in
  let max_lw = List.fold_left max neg_infinity log_ws in
  let sum    = List.fold_left
                 (fun acc lw -> acc +. exp (lw -. max_lw))
                 0.0 log_ws
  in
  log (sum /. n) +. max_lw

let categorical (probs : float list) : int =
  let u = Random.float 1.0 in
  let rec go i cumsum = function
    | []      -> i - 1
    | p :: ps ->
        let cumsum' = cumsum +. p in
        if u <= cumsum' then i
        else go (i + 1) cumsum' ps
  in
  go 0 0.0 probs

let mulpfilter (type a)
    (n     : int)
    (model : a model)
    : (a * float) list =
  let module PF = ParticleFilter(struct
    type nonrec a = a
    type w = pstate
  end) in
  let module Adv = MakeAdvance(struct
    type nonrec a = a
  end) in
  let step ((p, ps) : a model * pstate) : a model * pstate =
    let (ar, new_trace) =
      reuse_trace ps.trace (fun () ->
        default_sample (fun () ->
          Adv.advance ps.log_weight p))
    in
    match ar with
    | Finished { value; weight } ->
        ((fun () -> value),
         { ps with log_weight = weight; trace = new_trace })
    | Stepped { next_particle; weight } ->
        (next_particle,
         {log_weight = weight; trace = new_trace;
                   obs_idx = ps.obs_idx + 1 })
  in
  let handle_resample thunk =
    match thunk () with
    | v -> v
    | effect (PF.Resample pws), k ->
        let pstates = List.map snd pws in
        let ws      = List.map (fun ps -> ps.log_weight) pstates in
        let models  = List.map fst pws in
        let n       = List.length pws in
        let ws_norm = normalise ws in
        let w_mean  = log_mean_exp ws in
        let idxs    = List.init n (fun _ -> categorical ws_norm) in
        let resampled =
          List.map (fun i ->
            (List.nth models i,
             { (List.nth pstates i) with log_weight = w_mean }))
            idxs
        in
        Effect.Deep.continue k resampled
  in
  let raw = handle_resample (fun () ->
    PF.pfilter n empty_pstate step model)
  in
  List.map (fun (v, ps) -> (v, ps.log_weight)) raw