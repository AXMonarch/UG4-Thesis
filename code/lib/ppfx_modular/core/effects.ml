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

module FloatEffects : EffectsSig with type a = float = struct
  type a = float

  type _ Effect.t +=
    | Sample  : { addr : Types.address
                ; dist : Dist.t
                } -> float Effect.t

  type _ Effect.t +=
    | Observe : { addr : Types.address
                ; dist : Dist.t
                ; obs  : float
                } -> unit Effect.t
end

type 'a model = unit -> 'a