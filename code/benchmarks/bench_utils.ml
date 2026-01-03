let time f =
  let counter = Mtime_clock.counter () in
  f ();
  let elapsed = Mtime_clock.count counter in
  Mtime.Span.to_float_ns elapsed /. 1_000_000.0

let repeat_time n f =
  let total_time = ref 0.0 in
  for _ = 1 to n do
    total_time := !total_time +. time f
  done;
  !total_time /. float_of_int n

let print_row row =
  print_endline (String.concat "," row)
