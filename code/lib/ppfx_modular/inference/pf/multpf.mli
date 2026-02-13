(* Fig 11: Multinomial Particle Filter — public interface *)

val normalise      : float list -> float list
val log_mean_exp   : float list -> float
val categorical    : float list -> int
val handle_resample_mul : (unit -> 'a) -> 'a
val mulpfilter     : int -> float Effects.model -> (float * float) list