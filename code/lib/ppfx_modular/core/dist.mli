type t =
  | Uniform    of float * float
  | Normal     of float * float
  | Categorical of float array
  | Beta       of float * float
  | Bernoulli  of float
  | Binomial   of int * float

val draw     : t -> float -> float
val log_prob : t -> float -> float