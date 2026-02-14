(* advance.ml *)
open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) = struct
  type a = M.a

  let advance (w : float) (p : a model) : a advance_result =
    let rec handler : (a, a advance_result) Effect.Shallow.handler =
      { retc = (fun (x : a) ->
          Finished { value = x; weight = w })
      ; exnc = (fun e -> raise e)
      ; effc = fun (type c) (eff : c Effect.t) ->
          match eff with
          | FloatEffects.Observe { addr = _; dist; obs } ->
              Some (fun (k : (c, a) Effect.Shallow.continuation) ->
                let lp   = Dist.log_prob dist obs in
                let next = fun () ->
                  Effect.Shallow.continue_with k () handler
                in
                Stepped { next_particle = next
                        ; weight        = w +. lp })
          | _ -> None
      }
    in
    Effect.Shallow.continue_with
      (Effect.Shallow.fiber (fun (_ : unit) -> p ()))
      ()
      handler

  let rec to_particle (ar : a advance_result) : a model * float =
    match ar with
    | Finished { value; weight } ->
        ((fun () -> value), weight)
    | Stepped { next_particle; weight } ->
        ((fun () ->
            match to_particle (next_particle ()) with
            | (p, _) -> p ()), weight)
end