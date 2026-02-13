(* Fig 5, Fig 7, Fig 9: reusable model-level handlers      *)

open Types

(* Fig 5: handles Observe, returns observed value           *)
val default_observe : (unit -> 'a) -> 'a

(* Fig 5: handles Sample, draws fresh random value          *)
val default_sample  : (unit -> 'a) -> 'a

(* Fig 7: handles Sample, reuses trace or draws fresh       *)
(* returns (result, output_trace)                           *)
val reuse_trace     : trace -> (unit -> 'a) -> 'a * trace

(* Fig 9: intercepts Sample + Observe, records log probs    *)
(* rethrows effects — does not consume them                 *)
(* returns a new thunk that produces (result, lp_trace)     *)
val trace_lp        : (unit -> 'a) -> unit -> 'a * lp_trace