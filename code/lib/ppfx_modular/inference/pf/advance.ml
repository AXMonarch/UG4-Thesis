open Effects

type 'a advance_result =
  | Stepped  of { next_particle : 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) = struct
  type a = M.a

  let advance (w : float) (p : a model) : a advance_result =
    match p () with
    | x -> Finished { value = x; weight = w }
    | effect FloatEffects.Observe { addr = _; dist; obs }, k ->
        let lp = Dist.log_prob dist obs in
        Stepped { next_particle = Effect.Deep.continue k obs
                ; weight        = w +. lp }

  let suspend_after (n : int) (p : a model) : a =
    let t = ref n in
    match p () with
    | x -> x
    | effect FloatEffects.Observe { addr; dist; obs }, k ->
        if !t <= 0 then
          Effect.Deep.continue k obs
        else (t := !t - 1;
          Effect.Deep.continue k (Effect.perform (FloatEffects.Observe { addr; dist; obs })))

end