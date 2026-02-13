(* Fig 10: advance — shallow handler for particle stepping  *)
(*                                                          *)
(* advance :: LogP                                          *)
(*         -> Handler Observe es a                          *)
(*                    (Comp (Observe : es) a, LogP)         *)
(*                                                          *)
(* Val x case:           -> Finished                        *)
(* Left (Observe d y) :  -> Stepped, capture continuation   *)
(* Right op_es:          -> None, Sample falls through      *)

open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

let advance (w : float) (p : float model) : float advance_result =
  let rec handler : (float, float advance_result) Effect.Shallow.handler =
    { retc = (fun (x : float) ->
        (* Val x case from the paper                        *)
        Finished { value = x; weight = w })

    ; exnc = (fun e -> raise e)

    ; effc = fun (type c) (eff : c Effect.t) ->
        match eff with
        | FloatEffects.Observe { addr = _; dist; obs } ->
            (* Left (Observe d y) case from the paper       *)
            (* capture k, next_particle resumes it later    *)
            Some (fun (k : (c, float) Effect.Shallow.continuation) ->
              let lp   = Dist.log_prob dist obs in
              let next = fun () ->
                Effect.Shallow.continue_with k () handler
              in
              Stepped { next_particle = next
                      ; weight        = w +. lp })
        | _ ->
            (* Right op_es — Sample falls through           *)
            (* to outer default_sample handler              *)
            None
    }
  in
  Effect.Shallow.continue_with
    (Effect.Shallow.fiber (fun (_ : unit) -> p ()))
    ()
    handler

(* Converts advance_result back into (float model * float) *)
(* so pfilter's model_step type is satisfied               *)
(* Finished -> trivial thunk, done' detects termination    *)
(* Stepped  -> recursive thunk that keeps advancing        *)
let rec to_particle (ar : float advance_result) : float model * float =
  match ar with
  | Finished { value; weight } ->
      ((fun () -> value), weight)
  | Stepped { next_particle; weight } ->
      ((fun () ->
          match to_particle (next_particle ()) with
          | (p, _) -> p ()), weight)