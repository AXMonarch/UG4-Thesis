(* Fig 4 *)

module type EffectsSig = sig
  type a

  type _ Effect.t +=
    | Sample  : { addr : Types.address
                ; dist : Dist.t
                } -> a Effect.t

  type _ Effect.t +=
    | Observe : { addr : Types.address
                ; dist : Dist.t
                ; obs  : a
                } -> unit Effect.t
end

module FloatEffects : EffectsSig with type a = float

type 'a model = unit -> 'a