(* Fig 5, Fig 7, Fig 9: reusable model-level handlers      *)
(* These are the building blocks every inference algorithm  *)
(* composes — nothing here is algorithm-specific            *)
(*                                                          *)
(* Composition order for SSMH (Fig 9):                     *)
(*   reuse_trace τ (                                        *)
(*     default_observe (                                    *)
(*       trace_lp (                                         *)
(*         model)))                                         *)


(* -------------------------------------------------------- *)
(* Fig 5: defaultObserve                                    *)
(*                                                          *)
(* defaultObserve :: Handler Observe es a a                 *)
(* hop (Observe d y) k = k y                               *)
(*                                                          *)
(* Handles Observe by returning the observed value.         *)
(* Accumulates nothing — no log prob, no trace.             *)
(* -------------------------------------------------------- *)

open Effects
open Types

(* default_observe :: Handler Observe es a a                 *)
(* hop (Observe d y) k = k y                               *)
(* Handles Observe by returning the observed value.         *)

let default_observe (thunk : unit -> 'a) : 'a =
  match thunk () with
  | v -> v
  | effect (FloatEffects.Observe { obs = _; _ }), k ->
      Effect.Deep.continue k ()

(* -------------------------------------------------------- *)
(* Fig 5: defaultSample                                     *)
(*                                                          *)
(* defaultSample :: IO ∈ es => Handler Sample es a a        *)
(* hop (Sample d) k = do r <- call random; k (draw d r)    *)
(*                                                          *)
(* Handles Sample by drawing a fresh uniform value.         *)
(* IO drops — Random.float 1.0 is direct in OCaml.         *)
(* -------------------------------------------------------- *)
let default_sample (thunk : unit -> 'a) : 'a =
  match thunk () with
  | v -> v
  | effect (FloatEffects.Sample { dist; _ }), k ->
      let u     = Random.float 1.0 in
      let value = Dist.draw dist u in
      Effect.Deep.continue k value

(* -------------------------------------------------------- *)
(* Fig 7: reuseTrace                                        *)
(*                                                          *)
(* reuseTrace :: IO ∈ es => Trace -> Handler Sample es a    *)
(*                                         (a, Trace)       *)
(* hop τ (Sample d α) k =                                   *)
(*   do r <- call random                                    *)
(*      let (r', τ') = findOrInsert α r τ                   *)
(*      k τ' (draw d r')                                    *)
(*                                                          *)
(* If address exists in trace: reuse stored uniform value.  *)
(* If not: draw fresh uniform, insert into trace.           *)
(* Returns (result, output_trace).                          *)
(* -------------------------------------------------------- *)
let reuse_trace (tau : trace) (thunk : unit -> 'a) : 'a * trace =
  let out_trace = ref tau in
  let result =
    match thunk () with
    | v -> v
    | effect (FloatEffects.Sample { addr; dist }), k ->
        let u =
          match lookup_trace addr !out_trace with
          | Some existing -> existing          (* reuse stored value  *)
          | None          -> Random.float 1.0  (* draw fresh uniform  *)
        in
        let value = Dist.draw dist u in
        out_trace := insert_trace addr u !out_trace;
        Effect.Deep.continue k value
  in
  (result, !out_trace)

(* -------------------------------------------------------- *)
(* Fig 9: traceLP                                           *)
(*                                                          *)
(* traceLP :: (Observe ∈ es, Sample ∈ es)                   *)
(*         => Comp es a -> Comp es (a, LPTrace)             *)
(*                                                          *)
(* Not a handler — a computation transformer.               *)
(* Intercepts Sample and Observe, records logProb at each   *)
(* address, then rethrows so outer handlers deal with them. *)
(* Effect signature es is unchanged — effects pass through. *)
(* -------------------------------------------------------- *)
let trace_lp (thunk : unit -> 'a) : unit -> 'a * lp_trace =
  fun () ->
    let lp_acc = ref AddrMap.empty in
    let result =
      match thunk () with
      | v -> v
      | effect (FloatEffects.Sample { addr; dist }), k ->
          (* record log prob, rethrow for outer handler    *)
          let value = Effect.perform (FloatEffects.Sample { addr; dist }) in
          lp_acc := AddrMap.add addr (Dist.log_prob dist value) !lp_acc;
          Effect.Deep.continue k value
      | effect (FloatEffects.Observe { addr; dist; obs }), k ->
          (* record log prob, rethrow for outer handler    *)
          Effect.perform (FloatEffects.Observe { addr; dist; obs });
          lp_acc := AddrMap.add addr (Dist.log_prob dist obs) !lp_acc;
          Effect.Deep.continue k ()
    in
    (result, !lp_acc)