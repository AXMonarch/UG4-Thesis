type distribution =
  | Uniform of float * float
  | Normal of float * float

type _ Effect.t +=
  | Sample : { name : string; dist : distribution } -> float Effect.t
  | Observe : { name : string; dist : distribution; obs : float } -> unit Effect.t
  | Accept : unit -> bool Effect.t
  | Resample : float array -> int Effect.t

