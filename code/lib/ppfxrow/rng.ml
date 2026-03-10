let numbers : float array ref = ref [||]
let index : int ref = ref 0

let initialize (n : int) =
  Random.init 42;
  numbers := Array.init n (fun _ -> Random.float 1.0);
  index := 0

let next () =
  if !index >= Array.length !numbers then
    index := 0;
  let r = !numbers.(!index) in
  incr index;
  r

let load (path : string) =
  let ic = open_in path in
  let nums = ref [] in
  (try
    while true do
      nums := float_of_string (input_line ic) :: !nums
    done
  with End_of_file -> ());
  close_in ic;
  let arr = Array.of_list (List.rev !nums) in
  numbers := arr;
  index := 0

let generate_and_save (n : int) (path : string) =
  Random.init 42;  (* Fixed seed for reproducibility *)
  let oc = open_out path in
  for _ = 1 to n do
    Printf.fprintf oc "%.17g\n" (Random.float 1.0)
  done;
  close_out oc