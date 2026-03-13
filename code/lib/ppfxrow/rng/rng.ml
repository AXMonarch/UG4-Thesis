type _ Effect.t += Random : float Effect.t

let random_values = Rng_data.random_values

let handle_random : (unit -> 'a) -> 'a =
  fun f ->
  let next = ref (-1) in
  match f () with
  | ans -> ans
  | effect Random, k ->
      incr next;
      let val_ = Array.get random_values !next in
      Effect.Deep.continue k val_

let next () = Effect.perform Random