(* 
   Resample w a where
     Resample :: [(Model a, w)] -> Resample w [(Model a, w)]

   ModelStep w a = (Model a, w) -> IO (Model a, w)
   Drop the moadic style

   pfilter :: (Resample w ∈ fs, IO ∈ fs)
           => Int -> w -> ModelStep w a -> Model a 
           -> Comp fs [(a, w)]
*)

open Effects

(* Fig 10: type ModelStep w a = (Model a, w) -> IO (Model a, w) *)
(* IO drops — direct style in OCaml                             *)
type ('a, 'w) model_step = ('a model * 'w) -> ('a model * 'w)

(* generic module first, parametrize with concrete types later  *)
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

  (* Fig 10: done :: [(Model a, w)] -> Maybe [(a, w)]           *)
  (* Tries to run every particle thunk.                         *)
  (* If all return cleanly -> Some results                      *)
  (* If any particle performs an effect -> None                 *)
  (* This is the OCaml translation of matching Val x vs Op op k *)
  (* in the Haskell — we can't inspect the computation tree     *)
  (* directly, so we run and catch effects instead.             *)
  let done' (pws : (a model * w) list) : (a * w) list option =
    match List.map (fun (m, w) -> (m (), w)) pws with
    | ans           -> Some ans
    | effect _, _   -> None
    | exception _   -> None

  (* Fig 10: pfilter skeleton                                   *)
  (* Performs Resample but never handles it.                    *)
  (* The handler is always supplied from outside.               *)
  (*                                                            *)
  (* pfilter :: (Resample w ∈ fs, IO ∈ fs)                     *)
  (*         => Int -> w -> ModelStep w a -> Model a            *)
  (*         -> Comp fs [(a, w)]                                *)
  (*                                                            *)
  (* Comp fs drops — direct style, effects are native           *)
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