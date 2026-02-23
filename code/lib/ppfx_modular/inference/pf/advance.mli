open Effects

type 'a advance_result =
  | Stepped  of { next_particle : 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) : sig
  type a = M.a

  val advance       : float -> a model -> a advance_result
  val suspend_after : int -> a model -> a
end