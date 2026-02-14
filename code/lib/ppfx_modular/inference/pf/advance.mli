(* advance.mli *)
open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) : sig
  type a = M.a
  val advance     : float -> a model -> a advance_result
  val to_particle : a advance_result -> a model * float
end