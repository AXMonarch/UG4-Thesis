(* Fig 11: Pattern Instance: Multinomial Particle Filter    *)
(*                                                          *)
(* mulpfilter :: Int -> Model a -> IO [(a, LogP)]           *)
(* mulpfilter n = runIO                                     *)
(*             . handleResamplemul                          *)
(*             . pfilter n 0 stepModelmul                   *)

open Effects
open Model_handlers
open Pf

(* -------------------------------------------------------- *)
(* Weight arithmetic — Fig 11 auxiliaries                   *)
(* -------------------------------------------------------- *)

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
(* Fig 11: mulpfilter — general over 'a                     *)
(* Instantiates ParticleFilter and MakeAdvance locally      *)
(* so both Resample and advance work for any a              *)
(* -------------------------------------------------------- *)
let mulpfilter (type a)
    (n     : int)
    (model : a model)
    : (a * float) list =
  (* instantiate PF for this a *)
  let module PF = ParticleFilter(struct
    type nonrec a = a
    type w = float
  end) in
  (* instantiate advance for this a *)
  let module Adv = Advance.MakeAdvance(struct
    type nonrec a = a
  end) in
  (* Fig 11: stepModelmul                                   *)
  (* stepModelmul (p, w) = (runIO . defaultSample           *)
  (*                             . advance w) p             *)
  let step ((p, w) : a model * float) : a model * float =
    default_sample (fun () ->
      Adv.to_particle (Adv.advance w p))
  in
  (* Fig 11: handleResamplemul                              *)
  (* unzip, normalise, categorical, remap with mean weight  *)
  let handle_resample thunk =
    match thunk () with
    | v -> v
    | effect (PF.Resample pws), k ->
        let (ps, ws)  = List.split pws in
        let n         = List.length ps in
        let ws_norm   = normalise ws in
        let w_mean    = log_mean_exp ws in
        let idxs      = List.init n (fun _ -> categorical ws_norm) in
        let resampled = List.map
                          (fun i -> (List.nth ps i, w_mean))
                          idxs
        in
        Effect.Deep.continue k resampled
  in
  (* Fig 11: mulpfilter composition                         *)
  (* handleResamplemul . pfilter n 0 stepModelmul           *)
  handle_resample (fun () ->
    PF.pfilter n 0.0 step model)