open Effects

type ('a, 'w) model_step = ('a model * 'w) -> ('a model * 'w)

module type PFSig = sig
  type a
  type w

  type _ Effect.t +=
    | Resample : (a model * w) list -> (a model * w) list Effect.t
end

module ParticleFilter (M : sig type a type w end) = struct
  type a = M.a
  type w = M.w

  type _ Effect.t +=
    | Resample : (a model * w) list -> (a model * w) list Effect.t

  (* Fig 10: done :: [(Model a, w)] -> Maybe [(a, w)]       *)
  (* effect-catching detects whether particle has finished  *)
  (* Finished = returns cleanly                             *)
  (* Stepped  = performs an effect                          *)
  let done' (pws : (a model * w) list) : (a * w) list option =
    match List.map (fun (m, w) -> (m (), w)) pws with
    | ans         -> Some ans
    | effect _, _ -> None
    | exception _ -> None


  let pfilter
      (n     : int)
      (w0    : w)
      (step  : (a, w) model_step)
      (model : a model)
      : (a * w) list =
    let initial = List.init n (fun _ -> (model, w0)) in
    let rec pf_step (pws : (a model * w) list) : (a * w) list =
      let pws' = List.map step pws in
      match done' pws' with
      | Some results -> results
      | None         ->
          let resampled = Effect.perform (Resample pws') in
          pf_step resampled
    in
    pf_step initial
end