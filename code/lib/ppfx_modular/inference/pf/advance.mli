(* Fig 10: advance — shallow handler for particle stepping  *)
(* Fig 12: suspend_after — skip n observations then suspend *)

open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) : sig
  type a = M.a

  val advance       : float -> a model -> a advance_result

  val suspend_after : int -> float -> a model -> a advance_result
end