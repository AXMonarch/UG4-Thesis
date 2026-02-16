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

(* -------------------------------------------------------- *)
(* Fig 11: mulpfilter with w = pstate                       *)
(* -------------------------------------------------------- *)
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
  (* Fig 11: step_model_mul                                 *)
  (* replays model using reuse_trace + suspend_after        *)
  (* each step is fresh — no continuation aliasing          *)
  (* Sample effects handled by default_sample               *)
  let step ((p, ps) : a model * pstate) : a model * pstate =
    ignore p;
    (* replay model from beginning using stored trace       *)
    (* skip first ps.obs_idx observations                   *)
    (* suspend at observation ps.obs_idx + 1                *)
    let (ar, new_trace) =
      reuse_trace ps.trace (fun () ->
        default_sample (fun () ->
          Adv.suspend_after ps.obs_idx ps.log_weight model))
    in
    match ar with
    | Finished { value; weight } ->
        (* particle done — wrap as trivial thunk            *)
        ((fun () -> value),
         { log_weight = weight
         ; trace      = new_trace
         ; obs_idx    = ps.obs_idx })
    | Stepped { next_particle = _; weight } ->
    (* return original model — done' will run it           *)
    (* it will perform effects, done' returns None         *)
        (model,
        { log_weight = weight
        ; trace      = new_trace
        ; obs_idx    = ps.obs_idx + 1 })
  in
  (* Fig 11: handle_resample_mul                            *)
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
  (* Fig 11: mulpfilter composition                         *)
  (* extract log_weight from pstate for final result        *)
  let raw = handle_resample (fun () ->
    PF.pfilter n empty_pstate step model)
  in
  List.map (fun (v, ps) -> (v, ps.log_weight)) raw