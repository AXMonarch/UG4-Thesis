open Bench_utils
open Models
open Mh.Single_site_mh
open Mh.Particle_mh
open Pf.Multinomial_pf
open Pf.Resample_move_pf

type algorithm = SSMH | MPF | PMH | RMPF
type model_type = HMM | LinRegr

(* Data generation for HMM *)
let generate_hmm_observations n =
  Array.init n (fun _ -> Random.float 10.0)

(* Data generation for Linear Regression *)
let generate_linregr_data n =
  List.init n (fun i ->
    let x = float_of_int i in
    let y = 2.0 *. x +. 3.0 +. (Random.float 2.0 -. 1.0) in
    (x, y))

let simple_propose_fn _name _dist current_val =
  let step_size = 0.5 in
  let step = (Random.float (2.0 *. step_size)) -. step_size in
  Some (current_val +. step)

(* Benchmark functions for each algorithm *)

(* Single-Site Metropolis-Hastings *)
let bench_ssmh model_type seq_len num_iters =
  match model_type with
  | HMM ->
      let obs = generate_hmm_observations seq_len in
      let program () = Hmm.hidden_markov_model 5 obs in
      fun () ->
        let _ = run_single_site_mh program num_iters 0.5 in
        ()
  | LinRegr ->
      let data = generate_linregr_data seq_len in
      let program () = Linear_regression.linear_regression data in
      fun () ->
        let _ = run_single_site_mh program num_iters 0.5 in
        ()

(* Multinomial Particle Filter *)
let bench_mpf model_type seq_len num_particles =
  match model_type with
  | HMM ->
      let obs = generate_hmm_observations seq_len in
      let program _step () = Hmm.hidden_markov_model 5 obs in
      fun () ->
        let _ = run_multinomial_pf program num_particles seq_len 0.5 in
        ()
  | LinRegr ->
      let data = generate_linregr_data seq_len in
      let program _step () = Linear_regression.linear_regression data in
      fun () ->
        let _ = run_multinomial_pf program num_particles seq_len 0.5 in
        ()

(* Particle Metropolis-Hastings *)
let bench_pmh model_type seq_len num_iters num_particles =
  match model_type with
  | HMM ->
      let obs = generate_hmm_observations seq_len in
      let program () = Hmm.hidden_markov_model 5 obs in
      fun () ->
        let _ = run_particle_mh program num_iters num_particles simple_propose_fn in
        ()
  | LinRegr ->
      let data = generate_linregr_data seq_len in
      let program () = Linear_regression.linear_regression data in
      fun () ->
        let _ = run_particle_mh program num_iters num_particles simple_propose_fn in
        ()

(* Resample-Move Particle Filter *)
let bench_rmpf model_type seq_len num_particles num_moves =
  match model_type with
  | HMM ->
      let obs = generate_hmm_observations seq_len in
      let program _step () = Hmm.hidden_markov_model 5 obs in
      fun () ->
        let _ = run_resample_move_pf program num_particles seq_len num_moves simple_propose_fn 0.5 in
        ()
  | LinRegr ->
      let data = generate_linregr_data seq_len in
      let program _step () = Linear_regression.linear_regression data in
      fun () ->
        let _ = run_resample_move_pf program num_particles seq_len num_moves simple_propose_fn 0.5 in
        ()

(* Parse algorithm from string *)
let parse_algorithm s =
  match String.lowercase_ascii s with
  | "ssmh" -> SSMH
  | "mpf" -> MPF
  | "pmh" -> PMH
  | "rmpf" -> RMPF
  | _ -> failwith "Unknown algorithm. Use 'ssmh', 'mpf', 'pmh', or 'rmpf'"

(* Parse model from string *)
let parse_model s =
  match String.lowercase_ascii s with
  | "hmm" -> HMM
  | "linregr" | "linear" | "lr" -> LinRegr
  | _ -> failwith "Unknown model. Use 'hmm' or 'linregr'"

let parse_int_list s =
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.map int_of_string

let usage () =
  Printf.printf "Usage: bench_cli -alg <algorithm> -seqlens <len1,len2,...> [options]\n";
  Printf.printf "\nRequired arguments:\n";
  Printf.printf "  -alg <algorithm>      Algorithm to benchmark:\n";
  Printf.printf "                        'ssmh' - Single-Site Metropolis-Hastings\n";
  Printf.printf "                        'mpf'  - Multinomial Particle Filter\n";
  Printf.printf "                        'pmh'  - Particle Metropolis-Hastings\n";
  Printf.printf "                        'rmpf' - Resample-Move Particle Filter\n";
  Printf.printf "  -seqlens <lengths>    Comma-separated sequence lengths (e.g., 10,20,50)\n";
  Printf.printf "\nOptional arguments:\n";
  Printf.printf "  -model <type>         Model to benchmark (default: hmm):\n";
  Printf.printf "                        'hmm'     - Hidden Markov Model\n";
  Printf.printf "                        'linregr' - Linear Regression\n";
  Printf.printf "  -particles <counts>   Comma-separated particle counts (default varies by algorithm)\n";
  Printf.printf "                        SSMH: number of iterations (default: 100)\n";
  Printf.printf "                        MPF:  number of particles (default: 50,100)\n";
  Printf.printf "                        PMH:  number of particles (default: 10,20)\n";
  Printf.printf "                        RMPF: number of particles (default: 50,100)\n";
  Printf.printf "  -iters <counts>       For PMH: number of MH iterations (default: 100)\n";
  Printf.printf "  -moves <counts>       For RMPF: number of MCMC moves (default: 2,5)\n";
  Printf.printf "  -repeats <n>          Number of times to repeat each benchmark (default: 10)\n";
  Printf.printf "\nExamples:\n";
  Printf.printf "  bench_cli -alg ssmh -seqlens 10,20,50 -particles 50,100,200\n";
  Printf.printf "  bench_cli -alg mpf -seqlens 10,20,50 -particles 50,100 -model hmm\n";
  Printf.printf "  bench_cli -alg pmh -seqlens 10,20 -particles 10,20 -iters 100,200 -model linregr\n";
  Printf.printf "  bench_cli -alg rmpf -seqlens 10,20,50 -particles 50,100 -moves 2,5 -model hmm\n";
  exit 0

(* Parse command-line arguments *)
let parse_args () =
  let args = Array.to_list Sys.argv in
  let rec parse acc = function
    | [] -> acc
    | "-alg" :: alg :: rest ->
        parse (("alg", alg) :: acc) rest
    | "-model" :: mdl :: rest ->
        parse (("model", mdl) :: acc) rest
    | "-seqlens" :: lens :: rest ->
        parse (("seqlens", lens) :: acc) rest
    | "-particles" :: parts :: rest ->
        parse (("particles", parts) :: acc) rest
    | "-iters" :: its :: rest ->
        parse (("iters", its) :: acc) rest
    | "-moves" :: mvs :: rest ->
        parse (("moves", mvs) :: acc) rest
    | "-repeats" :: n :: rest ->
        parse (("repeats", n) :: acc) rest
    | "-h" :: _ | "--help" :: _ -> usage ()
    | _ :: rest -> parse acc rest
  in
  parse [] (List.tl args)

(* Run SSMH benchmarks *)
let run_ssmh_benchmarks model_type seq_lengths iter_counts num_repeats =
  Printf.printf "\n=== Single-Site Metropolis-Hastings Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "Iterations"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_iters ->
      let bench_fn = bench_ssmh model_type seq_len num_iters in
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
    ) iter_counts
  ) seq_lengths

(* Run MPF benchmarks *)
let run_mpf_benchmarks model_type seq_lengths particle_counts num_repeats =
  Printf.printf "\n=== Multinomial Particle Filter Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "Num_Particles"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_particles ->
      let bench_fn = bench_mpf model_type seq_len num_particles in
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

(* Run PMH benchmarks *)
let run_pmh_benchmarks model_type seq_lengths particle_counts iter_counts num_repeats =
  Printf.printf "\n=== Particle Metropolis-Hastings Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "Num_Particles"; "Iterations"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_particles ->
      List.iter (fun num_iters ->
        let bench_fn = bench_pmh model_type seq_len num_iters num_particles in
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
          string_of_int num_iters;
          Printf.sprintf "%.3f" avg_time;
          Printf.sprintf "%.3f" std_dev
        ]
      ) iter_counts
    ) particle_counts
  ) seq_lengths

(* Run RMPF benchmarks *)
let run_rmpf_benchmarks model_type seq_lengths particle_counts move_counts num_repeats =
  Printf.printf "\n=== Resample-Move Particle Filter Benchmarks ===\n\n";
  print_row ["Sequence_Length"; "Num_Particles"; "Num_Moves"; "Avg_Time_ms"; "Std_Dev_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_particles ->
      List.iter (fun num_moves ->
        let bench_fn = bench_rmpf model_type seq_len num_particles num_moves in
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
          string_of_int num_moves;
          Printf.sprintf "%.3f" avg_time;
          Printf.sprintf "%.3f" std_dev
        ]
      ) move_counts
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
      let model_type = 
        match List.assoc_opt "model" args with
        | Some m -> parse_model m
        | None -> HMM
      in
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
            | SSMH -> [100]
            | MPF -> [50; 100]
            | PMH -> [10; 20]
            | RMPF -> [50; 100]
      in
      
      let iter_counts =
        match List.assoc_opt "iters" args with
        | Some i -> parse_int_list i
        | None -> [100]
      in
      
      let move_counts =
        match List.assoc_opt "moves" args with
        | Some m -> parse_int_list m
        | None -> [2; 5]
      in
      
      Printf.printf "Benchmark Configuration:\n";
      Printf.printf "  Model: %s\n" (match model_type with HMM -> "Hidden Markov Model" | LinRegr -> "Linear Regression");
      Printf.printf "  Algorithm: %s\n" 
        (match algorithm with 
         | SSMH -> "Single-Site MH" 
         | MPF -> "Multinomial PF" 
         | PMH -> "Particle MH"
         | RMPF -> "Resample-Move PF");
      Printf.printf "  Sequence lengths: %s\n" (String.concat ", " (List.map string_of_int seq_lengths));
      Printf.printf "  Particle counts: %s\n" (String.concat ", " (List.map string_of_int particle_counts));
      (match algorithm with
       | PMH -> Printf.printf "  Iteration counts: %s\n" (String.concat ", " (List.map string_of_int iter_counts))
       | RMPF -> Printf.printf "  Move counts: %s\n" (String.concat ", " (List.map string_of_int move_counts))
       | _ -> ());
      Printf.printf "  Repeats per configuration: %d\n" num_repeats;
      
      match algorithm with
      | SSMH -> run_ssmh_benchmarks model_type seq_lengths particle_counts num_repeats
      | MPF -> run_mpf_benchmarks model_type seq_lengths particle_counts num_repeats
      | PMH -> run_pmh_benchmarks model_type seq_lengths particle_counts iter_counts num_repeats
      | RMPF -> run_rmpf_benchmarks model_type seq_lengths particle_counts move_counts num_repeats
