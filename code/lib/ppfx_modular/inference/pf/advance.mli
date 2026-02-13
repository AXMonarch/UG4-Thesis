(* Fig 10: advance — shallow handler for particle stepping  *)

open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

val advance     : float -> 'a model -> 'a advance_result
val to_particle : 'a advance_result -> 'a model * float