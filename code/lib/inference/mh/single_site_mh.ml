[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base

type variable_selector = trace -> string option
let random_variable_selector trace =
  let keys = Hashtbl.fold (fun k _ acc -> k :: acc) trace.choices [] in
  if keys = [] then None
  else Some (List.nth keys (Random.int (List.length keys)))

let make_single_site_propose (var_selector : variable_selector) (step_size : float) =
  fun name dist current_val ->
    let step = (Random.float (2.0 *. step_size)) -. step_size in
    Some (current_val +. step)

let single_site_mh_kernel (type a)
    (program : unit -> a)
    (current_trace : trace)
    (var_selector : variable_selector)
    (step_size : float) : trace * bool =
  let selected_var = var_selector current_trace in
  match selected_var with
  | None -> (current_trace, false)
  | Some _var ->
      let propose_fn = make_single_site_propose var_selector step_size in
      mh_kernel program current_trace propose_fn

let run_single_site_mh (type a)
    (program : unit -> a)
    (num_iterations : int)
    (step_size : float) : trace list * int =
  let (initial_trace, _) = forward_sample program in
  
  let rec iterate n current traces accepted_count =
    if n >= num_iterations then
      (List.rev traces, accepted_count)
    else
      let (next_trace, was_accepted) = 
        single_site_mh_kernel program current random_variable_selector step_size in
      let new_accepted = if was_accepted then accepted_count + 1 else accepted_count in
      iterate (n + 1) next_trace (next_trace :: traces) new_accepted
  in
  
  iterate 0 initial_trace [] 0

let demo_single_site_mh () =
  print_endline "\n=== Single-Site MH Demo ===";
  
  let two_var_model () =
    let x = perform (Sample { name = "x"; dist = Normal (0.0, 1.0) }) in
    let y = perform (Sample { name = "y"; dist = Normal (0.0, 1.0) }) in
    perform (Observe { name = "obs_x"; dist = Normal (x, 0.5); obs = 1.0 });
    perform (Observe { name = "obs_y"; dist = Normal (y, 0.5); obs = 2.0 });
    (x, y)
  in
  
  let (traces, accepted) = run_single_site_mh two_var_model 300 0.3 in
  let acceptance_rate = float_of_int accepted /. 300.0 in
  
  let samples = List.filteri (fun i _ -> i >= 150) traces in
  let x_samples = List.filter_map (fun t -> Hashtbl.find_opt t.choices "x") samples in
  let y_samples = List.filter_map (fun t -> Hashtbl.find_opt t.choices "y") samples in
  let avg_x = List.fold_left (+.) 0.0 x_samples /. float_of_int (List.length x_samples) in
  let avg_y = List.fold_left (+.) 0.0 y_samples /. float_of_int (List.length y_samples) in
  
  Printf.printf "Single-Site MH: %d iterations, %.1f%% acceptance\n" 300 (acceptance_rate *. 100.0);
  Printf.printf "Estimated x = %.3f (obs 1.0), y = %.3f (obs 2.0)\n" avg_x avg_y;
  print_endline "=== End Single-Site MH Demo ===\n"
