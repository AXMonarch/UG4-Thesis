[@@@ocaml.warning "-27-33-39"]

open Effect
open Effects
open Mh_base
open Models

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
  print_endline "\n=== Single-Site MH Demo: HMM ===";
  
  let observations = [| 1.0; 2.0; 2.5; 3.0 |] in
  let num_states = 3 in
  
  let hmm_model () = Hmm.hidden_markov_model num_states observations in
  
  let (traces, accepted) = run_single_site_mh hmm_model 250 0.5 in
  let acceptance_rate = float_of_int accepted /. 250.0 in
  let samples = List.filteri (fun i _ -> i >= 125) traces in
  let state_estimates = List.init (Array.length observations) (fun t ->
    let state_key = "state_" ^ string_of_int t in
    let vals = List.filter_map (fun trace -> Hashtbl.find_opt trace.choices state_key) samples in
    if vals = [] then 0.0 else List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals)
  ) in
  
  Printf.printf "Single-Site MH-HMM: %d iterations, %.1f%% acceptance\n" 250 (acceptance_rate *. 100.0);
  Printf.printf "Observations: [%.1f; %.1f; %.1f; %.1f]\n" 
    observations.(0) observations.(1) observations.(2) observations.(3);
  Printf.printf "Estimated states: [";
  List.iteri (fun i est -> if i > 0 then Printf.printf "; "; Printf.printf "%.1f" est) state_estimates;
  Printf.printf "]\n";
  print_endline "=== End Single-Site MH Demo ===\n"
