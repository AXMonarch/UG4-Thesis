(* Fig 11: Pattern Instance: Multinomial Particle Filter *)
(*
   mulpfilter :: Int -> Model a -> IO [(a, LogP)]
   mulpfilter n = runIO . handleResamplemul . pfilter n 0 stepModelmul

   handleResamplemul :: IO ∈ fs => Handler (Resample LogP) fs a a

   stepModelmul :: ModelStep LogP a
   stepModelmul (p, w) = (runIO . defaultSample . advance w) p
*)

open Model_handlers
open Advance
open Pf

(* Concrete instantiation — w = float (LogP), a = float *)
module FloatPF = ParticleFilter(struct
  type a = float
  type w = float
end)

(* -------------------------------------------------------- *)
(* Weight arithmetic — Fig 11 auxiliaries                   *)
(* -------------------------------------------------------- *)

(* normalise :: [LogP] -> [LogP]                            *)
(* converts log weights to normalised probabilities         *)
let normalise (log_ws : float list) : float list =
  let max_lw  = List.fold_left max neg_infinity log_ws in
  let shifted = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
  let total   = List.fold_left ( +. ) 0.0 shifted in
  List.map (fun s -> s /. total) shifted

(* log_mean_exp :: [LogP] -> LogP                           *)
(* log of the mean of the exponentiated weights             *)
(* = log(1/n * sum(exp(w_i)))                               *)
let log_mean_exp (log_ws : float list) : float =
  let n      = float_of_int (List.length log_ws) in
  let max_lw = List.fold_left max neg_infinity log_ws in
  let sum    = List.fold_left
                 (fun acc lw -> acc +. exp (lw -. max_lw))
                 0.0 log_ws
  in
  log (sum /. n) +. max_lw

(* categorical :: [LogP] -> int                             *)
(* samples an index proportional to normalised weights      *)
let categorical (probs : float list) : int =
  let u   = Random.float 1.0 in
  let rec go i cumsum = function
    | []      -> i - 1
    | p :: ps ->
        let cumsum' = cumsum +. p in
        if u <= cumsum' then i
        else go (i + 1) cumsum' ps
  in
  go 0 0.0 probs

(* step_model_mul :: ModelStep LogP a                       *)
(* Fig 11: advances one particle to its next Observe        *)
(* using defaultSample for Sample effects                   *)
(* and advance for the Observe suspension                   *)
let step_model_mul
    ((p, w) : float Effects.model * float)
    : float Effects.model * float =
  default_sample (fun () ->
    to_particle (advance w p))
    
(* -------------------------------------------------------- *)
(* Fig 11: handleResamplemul                                *)
(* The concrete Resample handler for Multinomial PF         *)
(*                                                          *)
(* handleResamplemul :: IO ∈ fs                             *)
(*   => Handler (Resample LogP) fs a a                      *)
(*                                                          *)
(* hop (Resample pws) k =                                   *)
(*   let (ps, ws)       = unzip pws                         *)
(*       (ws_norm, w_s) = (normalise ws, logMeanExp ws)     *)
(*       idxs           = replicateM n (categorical ws_norm)*)
(*   in k (map ((, w_s) . (ps !!)) idxs)                    *)
(* -------------------------------------------------------- *)
let handle_resample_mul (thunk : unit -> 'a) : 'a =
  match thunk () with
  | v -> v
  | effect (FloatPF.Resample pws), k ->
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

(* -------------------------------------------------------- *)
(* Fig 11: mulpfilter                                       *)
(* The complete algorithm —                                 *)
(* skeleton + handler + stepper wired together              *)
(*                                                          *)
(* mulpfilter n = runIO                                     *)
(*              . handleResamplemul                         *)
(*              . pfilter n 0 stepModelmul                  *)
(* -------------------------------------------------------- *)
let mulpfilter (n : int) (model : float Effects.model) : (float * float) list =
  handle_resample_mul (fun () ->
    FloatPF.pfilter n 0.0 step_model_mul model)