(* Fig 10: advance — shallow handler for particle stepping  *)
(* Fig 12: suspend_after — skip n observations then suspend *)

open Effects

type 'a advance_result =
  | Stepped  of { next_particle : unit -> 'a advance_result
                ; weight        : float }
  | Finished of { value         : 'a
                ; weight        : float }

module MakeAdvance (M : sig type a end) = struct
  type a = M.a

  (* Fig 10: advance                                        *)
  (* runs particle forward to next Observe                  *)
  (* Sample effects fall through to outer default_sample    *)
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
          | _ ->
              None
      }
    in
    Effect.Shallow.continue_with
      (Effect.Shallow.fiber (fun (_ : unit) -> p ()))
      ()
      handler

  (* Fig 12: suspend_after                                  *)
  (*                                                        *)
  (* suspendAfter :: Observe ∈ es                           *)
  (*   => Int -> Comp es a -> Comp es (Comp es a)           *)
  (*                                                        *)
  (* Replays model skipping first n Observe calls           *)
  (* then suspends at observation n+1                       *)
  (* Threads skip counter and accumulated weight            *)
  (* through handler — mirrors paper's threading of t       *)
  let suspend_after (n : int) (w : float) (p : a model)
      : a advance_result =
    let rec make_handler (skip : int) (acc_w : float)
        : (a, a advance_result) Effect.Shallow.handler =
      { retc = (fun (x : a) ->
          (* Val x case — particle finished                 *)
          Finished { value = x; weight = acc_w })

      ; exnc = (fun e -> raise e)

      ; effc = fun (type c) (eff : c Effect.t) ->
          match eff with
          | FloatEffects.Observe { addr = _; dist; obs } ->
              Some (fun (k : (c, a) Effect.Shallow.continuation) ->
                let lp = Dist.log_prob dist obs in
                if skip > 0 then
                  (* skip this observation —                *)
                  (* continue with decremented counter      *)
                  Effect.Shallow.continue_with k ()
                    (make_handler (skip - 1) (acc_w +. lp))
                else
                  (* observation n reached — suspend here   *)
                  let next = fun () ->
                    Effect.Shallow.continue_with k ()
                      (make_handler 0 (acc_w +. lp))
                  in
                  Stepped { next_particle = next
                           ; weight        = acc_w +. lp })
          | _ ->
              None
      }
    in
    Effect.Shallow.continue_with
      (Effect.Shallow.fiber (fun (_ : unit) -> p ()))
      ()
      (make_handler n w)
end