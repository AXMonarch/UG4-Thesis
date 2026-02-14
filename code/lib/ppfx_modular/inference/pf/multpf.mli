(* Fig 11: Multinomial Particle Filter — public interface   *)

open Effects

val normalise    : float list -> float list
val log_mean_exp : float list -> float
val categorical  : float list -> int

(* Fig 11: general mulpfilter — works for any 'a model      *)
(* w is always float (LogP)                                 *)
val mulpfilter : int -> 'a model -> ('a * float) list