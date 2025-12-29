open Bench_utils
open Models
open Mh.Mh_base
open Pf.Pf_base

type algorithm = MH | PF

let generate_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let simple_propose_fn _name _dist current_val =
  let step_size = 0.5 in
  let step = (Random.float (2.0 *. step_size)) -. step_size in
  Some (current_val +. step)

let simple_resample particles =
  let n = Array.length particles in
  let normalized = normalize_weights particles in
  let cumulative = Array.make n 0.0 in
  cumulative.(0) <- normalized.(0).weight;
  for i = 1 to n - 1 do
    cumulative.(i) <- cumulative.(i - 1) +. normalized.(i).weight
  done;
  let new_particles = Array.init n (fun _ ->
    let r = Random.float 1.0 in
    let rec find_index i =
      if i >= n - 1 || r <= cumulative.(i) then i
      else find_index (i + 1)
    in
    let idx = find_index 0 in
    copy_particle normalized.(idx)
  ) in
  reset_weights new_particles

let bench_mh seq_len num_iters =
  let obs = generate_observations seq_len in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_mh program num_iters simple_propose_fn in
    ()

let bench_pf seq_len num_particles =
  let obs = generate_observations seq_len in
  let program _step () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_pf program num_particles seq_len simple_resample 0.5 in
    ()

(* Parse algorithm from string *)
let parse_algorithm s =
  match String.lowercase_ascii s with
  | "mh" -> MH
  | "pf" -> PF
  | _ -> failwith "Unknown algorithm. Use 'mh' or 'pf'"

let parse_int_list s =
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.map int_of_string

let usage () =
  Printf.printf "Usage: bench_cli -alg <mh|pf> -seqlens <len1,len2,...> [options]\n";
  Printf.printf "\nRequired arguments:\n";
  Printf.printf "  -alg <algorithm>      Algorithm to benchmark: 'mh' or 'pf'\n";
  Printf.printf "  -seqlens <lengths>    Comma-separated sequence lengths (e.g., 10,20,50)\n";
  Printf.printf "\nOptional arguments:\n";
  Printf.printf "  -particles <counts>   Comma-separated particle/iteration counts\n";
  Printf.printf "                        For MH: number of iterations (default: 100)\n";
  Printf.printf "                        For PF: number of particles (default: 50,100)\n";
  Printf.printf "  -repeats <n>          Number of times to repeat each benchmark (default: 10)\n";
  Printf.printf "\nExamples:\n";
  Printf.printf "  bench_cli -alg mh -seqlens 10,20,50 -particles 50,100,200\n";
  Printf.printf "  bench_cli -alg pf -seqlens 10,20,50,100 -particles 50,100\n";
  Printf.printf "  bench_cli -alg mh -seqlens 10,20 -repeats 20\n";
  exit 0

(* Parse command-line arguments *)
let parse_args () =
  let args = Array.to_list Sys.argv in
  let rec parse acc = function
    | [] -> acc
    | "-alg" :: alg :: rest ->
        parse (("alg", alg) :: acc) rest
    | "-seqlens" :: lens :: rest ->
        parse (("seqlens", lens) :: acc) rest
    | "-particles" :: parts :: rest ->
        parse (("particles", parts) :: acc) rest
    | "-repeats" :: n :: rest ->
        parse (("repeats", n) :: acc) rest
    | "-h" :: _ | "--help" :: _ -> usage ()
    | _ :: rest -> parse acc rest
  in
  parse [] (List.tl args)

(* Run MH benchmarks *)
let run_mh_benchmarks seq_lengths particle_counts num_repeats =
  Printf.printf "\n=== Metropolis-Hastings Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "MH_Iterations"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_iters ->
      let bench_fn = bench_mh seq_len num_iters in
      let times = Array.init num_repeats (fun _ -> time bench_fn) in
      let avg_time = Array.fold_left (+.) 0.0 times /. float_of_int num_repeats in
      let variance = 
        Array.fold_left (fun acc t -> acc +. (t -. avg_time) ** 2.0) 0.0 times 
        /. float_of_int num_repeats 
      in
      let std_dev = sqrt variance in
      print_row [
        string_of_int seq_len;
        string_of_int num_iters;
        Printf.sprintf "%.3f" avg_time;
        Printf.sprintf "%.3f" std_dev
      ]
    ) particle_counts
  ) seq_lengths

(* Run PF benchmarks *)
let run_pf_benchmarks seq_lengths particle_counts num_repeats =
  Printf.printf "\n=== Particle Filter Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "Num_Particles"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_particles ->
      let bench_fn = bench_pf seq_len num_particles in
      let times = Array.init num_repeats (fun _ -> time bench_fn) in
      let avg_time = Array.fold_left (+.) 0.0 times /. float_of_int num_repeats in
      let variance = 
        Array.fold_left (fun acc t -> acc +. (t -. avg_time) ** 2.0) 0.0 times 
        /. float_of_int num_repeats 
      in
      let std_dev = sqrt variance in
      print_row [
        string_of_int seq_len;
        string_of_int num_particles;
        Printf.sprintf "%.3f" avg_time;
        Printf.sprintf "%.3f" std_dev
      ]
    ) particle_counts
  ) seq_lengths

(* Main entry point *)
let () =
  let args = parse_args () in
  
  (* Check for required arguments *)
  if List.length args = 0 then usage ();
  
  let alg_opt = List.assoc_opt "alg" args in
  let seqlens_opt = List.assoc_opt "seqlens" args in
  
  match (alg_opt, seqlens_opt) with
  | (None, _) | (_, None) ->
      Printf.eprintf "Error: Both -alg and -seqlens are required\n\n";
      usage ()
  | (Some alg_str, Some seqlens_str) ->
      let algorithm = parse_algorithm alg_str in
      let seq_lengths = parse_int_list seqlens_str in
      let num_repeats = 
        match List.assoc_opt "repeats" args with
        | Some n -> int_of_string n
        | None -> 10
      in
      
      let particle_counts =
        match List.assoc_opt "particles" args with
        | Some p -> parse_int_list p
        | None -> 
            match algorithm with
            | MH -> [100]
            | PF -> [50; 100]
      in
      
      Printf.printf "Benchmark Configuration:\n";
      Printf.printf "  Algorithm: %s\n" (match algorithm with MH -> "MH" | PF -> "PF");
      Printf.printf "  Sequence lengths: %s\n" (String.concat ", " (List.map string_of_int seq_lengths));
      Printf.printf "  %s: %s\n" 
        (match algorithm with MH -> "MH iterations" | PF -> "Particle counts")
        (String.concat ", " (List.map string_of_int particle_counts));
      Printf.printf "  Repeats per configuration: %d\n" num_repeats;
      
      match algorithm with
      | MH -> run_mh_benchmarks seq_lengths particle_counts num_repeats
      | PF -> run_pf_benchmarks seq_lengths particle_counts num_repeats
