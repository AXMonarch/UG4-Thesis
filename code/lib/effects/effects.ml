type distribution =
  | Uniform of float * float
  | Normal of float * float

(** Trace type for inference operations *)
type trace = {
  choices : (string, float) Hashtbl.t;
  mutable log_prob : float;
}

(** State type for inference (result, weight, trace) *)
type 'a state = 'a * float * trace

(** Probabilistic programming effects *)
type _ Effect.t +=
  | Sample : { name : string; dist : distribution } -> float Effect.t
  | Observe : { name : string; dist : distribution; obs : float } -> unit Effect.t

(** Inference operation effects *)
type _ Effect.t +=
  | Propose : trace -> trace Effect.t
  | Accept : ('a state * 'a state) -> 'a state Effect.t
  | Resample : float array -> int Effect.t

(* Propose takes a trace and returns a new trace if the proposal is accepted *)
(* Accept takes two states and returns the accepted state if the proposal is accepted based on the acceptance ratio*)
(* Resample takes an array of floats and returns the index of the resampled element *)
(* We resample because we want to obtain a new sample from the proposal distribution *)