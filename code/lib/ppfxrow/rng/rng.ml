type _ Effect.t += Random : float Effect.t

let random_values = Rng_data.random_values

let current_pos = ref (-1)

let handle_random : (unit -> 'a) -> 'a =
  fun f ->
  current_pos := -1;
  match f () with
  | ans -> ans
  | effect Random, k ->
      incr current_pos;
      let val_ = Array.get random_values !current_pos in
      Effect.Deep.continue k val_

let next () = Effect.perform Random

let snapshot () = !current_pos + 1