type t = int

let make =
  let next = ref (-1) in
  fun () ->
    incr next; !next