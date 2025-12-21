let time f =
  let start = Sys.time () in
  f ();
  let stop = Sys.time () in
  (stop -. start) *. 1000.0

let repeat_time n f =
  let total_time = ref 0.0 in
  for _ = 1 to n do
    total_time := !total_time +. time f
  done;
  !total_time /. float_of_int n

let print_row row =
  print_endline (String.concat "," row)
