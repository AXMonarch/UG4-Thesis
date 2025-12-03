open Effects

(** Get a specific variable from a trace *)
let get_variable trace name =
  Hashtbl.find_opt trace.choices name

(** Convert trace to association list *)
let trace_to_list trace =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) trace.choices []

(** Print trace information *)
let print_trace trace =
  Printf.printf "Trace (log_prob = %.4f):\n" trace.log_prob;
  Hashtbl.iter (fun name value ->
    Printf.printf "  %s = %.4f\n" name value
  ) trace.choices

(** Compute acceptance rate *)
let acceptance_rate accepted total =
  float_of_int accepted /. float_of_int total

(** Extract samples for a specific variable from trace list *)
let extract_samples traces var_name =
  List.filter_map (fun trace -> get_variable trace var_name) traces

(** Compute statistics from traces *)
let trace_statistics traces var_name =
  let samples = extract_samples traces var_name in
  let mean = Stats.mean samples in
  let std = Stats.std_dev samples in
  (mean, std)

(** Print summary statistics for all variables in traces *)
let print_summary traces =
  let all_vars = match traces with
    | [] -> []
    | first :: _ -> trace_to_list first |> List.map fst
  in
  Printf.printf "\n=== MH Summary Statistics ===\n";
  List.iter (fun var_name ->
    let (mean, std) = trace_statistics traces var_name in
    Printf.printf "%s: mean = %.4f, std = %.4f\n" var_name mean std
  ) all_vars;
  Printf.printf "============================\n"
