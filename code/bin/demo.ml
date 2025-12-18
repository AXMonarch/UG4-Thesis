[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Multicont.Deep

type _ Effect.t += Flip : bool Effect.t

let model() = 
  let a = Effect.perform Flip in 
  let b = Effect.perform Flip in
  if a || b then 1 else 0

let flip_handler f =
  match f () with
  | v -> [v]
  | effect Flip, k ->
      let r = promote k in (*Here, r stores the continuation that can be resumed multiple times*)
      let t = resume r true in (* Here, t is the result of resuming the continuation with true *)
      let f = resume r false in (* Here, f is the result of resuming the continuation with false *)
      t @ f 

let () =
  let results = flip_handler model in
  Printf.printf "Results: [%s]\n" (String.concat "; " (List.map string_of_int results))
