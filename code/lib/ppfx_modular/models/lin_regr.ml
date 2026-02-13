(* Linear regression model — directly from paper introduction *)
(*                                                             *)
(* linRegr :: Double → Double → Model (Double, Double)        *)
(* linRegr x y = do                                           *)
(*   m ← call (Sample (Normal 0 3))                           *)
(*   c ← call (Sample (Normal 0 2))                           *)
(*   call (Observe (Normal (m ∗ x + c) 1) y)                  *)
(*   pure (m, c)                                              *)

open Effects
open Types

(* fresh_addr: per-execution counter                          *)
(* reset on every model call so addresses are consistent      *)
(* across particles and resampling steps                      *)
let fresh_addr (counter : int ref) (tag : string) : address =
  let local = !counter in
  incr counter;
  make_addr tag local

(* lin_regr x y : float model                                 *)
(* returns (m, c) — slope and intercept                       *)
(* one Observe conditions on the data point (x, y)            *)
let lin_regr (x : float) (y : float) : (float * float) model =
  fun () ->
    let counter = ref 0 in
    let m = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "m"
              ; dist = Dist.Normal (0.0, 3.0) }) in
    let c = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "c"
              ; dist = Dist.Normal (0.0, 2.0) }) in
    Effect.perform (FloatEffects.Observe
              { addr = fresh_addr counter "obs"
              ; dist = Dist.Normal (m *. x +. c, 1.0)
              ; obs  = y });
    (m, c)