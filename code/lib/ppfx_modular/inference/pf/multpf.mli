open Effects

val normalise    : float list -> float list
val log_mean_exp : float list -> float
val categorical  : float list -> int

val mulpfilter : int -> 'a model -> ('a * float) list