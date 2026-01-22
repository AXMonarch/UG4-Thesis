(*

  Experiment 1: Varying Dataset Size (Fixed Algorithm Parameters)
 
  Experiment 2: Varying Algorithm Parameters (Fixed Dataset Size)

*)

open Bench_utils
open Models
open Pf.Multinomial_pf
open Pf.Resample_move_pf

type pf_variant = MultinomialPF | ResampleMovePF
type model_type = HMM | LinRegr

let generate_hmm_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let generate_linregr_data n =
  List.init n (fun i ->
    let x = float_of_int i in
    let y = 2.0 *. x +. 3.0 +. (Random.float 2.0 -. 1.0) in
    (x, y))

let num_repeats = 10

module Experiment1 = struct
  let fixed_particles = 100
  let fixed_resample_threshold = 0.5
  let fixed_rmpf_mh_moves = 2
  let fixed_rmpf_step_size = 0.5
  let dataset_sizes = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500]

  let bench_multinomial_pf model_type size =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_multinomial_pf program fixed_particles fixed_resample_threshold in
          ()
    | LinRegr ->
        let data = generate_linregr_data size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_multinomial_pf program fixed_particles fixed_resample_threshold in
          ()

  let bench_resample_move_pf model_type size =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_resample_move_pf program fixed_particles fixed_rmpf_mh_moves 
                    fixed_rmpf_step_size fixed_resample_threshold in
          ()
    | LinRegr ->
        let data = generate_linregr_data size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_resample_move_pf program fixed_particles fixed_rmpf_mh_moves 
                    fixed_rmpf_step_size fixed_resample_threshold in
          ()

  let bench_variant variant model_type size =
    match variant with
    | MultinomialPF -> bench_multinomial_pf model_type size
    | ResampleMovePF -> bench_resample_move_pf model_type size

  let variant_to_string = function
    | MultinomialPF -> "MultinomialPF"
    | ResampleMovePF -> "ResampleMovePF"

  let model_to_string = function
    | HMM -> "HMM"
    | LinRegr -> "LinRegr"

  let run_experiment variant model_type =
    let variant_name = variant_to_string variant in
    let model_name = model_to_string model_type in
    let filename = Printf.sprintf "varying_dataset_%s_%s.csv" 
      (String.lowercase_ascii variant_name) 
      (String.lowercase_ascii model_name) in
    
    Printf.printf "Running Experiment 1 (%s on %s)...\n" variant_name model_name;
    
    let oc = open_out ("benchmark_results/" ^ filename) in
    let print_to_file row =
      output_string oc (String.concat "," row ^ "\n")
    in
    
    print_to_file ["Dataset_Size"; "Avg_Time_ms"; "Fixed_Particles"];
    
    List.iter (fun size ->
      let bench_fn = bench_variant variant model_type size in
      let avg_time = repeat_time num_repeats bench_fn in
      print_to_file [
        string_of_int size;
        Printf.sprintf "%.2f" avg_time;
        string_of_int fixed_particles
      ];
      Printf.printf "  Size %d: %.2f ms\n" size avg_time;
      flush stdout
    ) dataset_sizes;
    
    close_out oc;
    Printf.printf "Results saved to benchmark_results/%s\n\n" filename
end

(* ========== EXPERIMENT 2: Varying Algorithm Parameters ========== *)
module Experiment2 = struct
  let fixed_hmm_size = 50
  let fixed_linregr_size = 50
  let fixed_resample_threshold = 0.5
  let fixed_rmpf_mh_moves = 2
  let fixed_rmpf_step_size = 0.5
  let particle_counts = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500]

  let bench_multinomial_pf model_type num_particles =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations fixed_hmm_size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_multinomial_pf program num_particles fixed_resample_threshold in
          ()
    | LinRegr ->
        let data = generate_linregr_data fixed_linregr_size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_multinomial_pf program num_particles fixed_resample_threshold in
          ()

  let bench_resample_move_pf model_type num_particles =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations fixed_hmm_size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_resample_move_pf program num_particles fixed_rmpf_mh_moves 
                    fixed_rmpf_step_size fixed_resample_threshold in
          ()
    | LinRegr ->
        let data = generate_linregr_data fixed_linregr_size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_resample_move_pf program num_particles fixed_rmpf_mh_moves 
                    fixed_rmpf_step_size fixed_resample_threshold in
          ()

  let bench_variant variant model_type num_particles =
    match variant with
    | MultinomialPF -> bench_multinomial_pf model_type num_particles
    | ResampleMovePF -> bench_resample_move_pf model_type num_particles

  let variant_to_string = function
    | MultinomialPF -> "MultinomialPF"
    | ResampleMovePF -> "ResampleMovePF"

  let model_to_string = function
    | HMM -> "HMM"
    | LinRegr -> "LinRegr"

  let run_experiment variant model_type =
    let variant_name = variant_to_string variant in
    let model_name = model_to_string model_type in
    let filename = Printf.sprintf "varying_particles_%s_%s.csv" 
      (String.lowercase_ascii variant_name) 
      (String.lowercase_ascii model_name) in
    
    Printf.printf "Running Experiment 2 (%s on %s)...\n" variant_name model_name;
    
    let oc = open_out ("benchmark_results/" ^ filename) in
    let print_to_file row =
      output_string oc (String.concat "," row ^ "\n")
    in
    
    print_to_file ["Num_Particles"; "Avg_Time_ms"; "Fixed_Dataset_Size"];
    
    List.iter (fun num_particles ->
      let bench_fn = bench_variant variant model_type num_particles in
      let avg_time = repeat_time num_repeats bench_fn in
      let fixed_size = match model_type with
        | HMM -> fixed_hmm_size
        | LinRegr -> fixed_linregr_size
      in
      print_to_file [
        string_of_int num_particles;
        Printf.sprintf "%.2f" avg_time;
        string_of_int fixed_size
      ];
      Printf.printf "  Particles %d: %.2f ms\n" num_particles avg_time;
      flush stdout
    ) particle_counts;
    
    close_out oc;
    Printf.printf "Results saved to benchmark_results/%s\n\n" filename
end

let () =
  Random.init 42;
  Printf.printf "\n=== PF Variants Benchmark Suite ===\n\n";
  
  let all_variants = [MultinomialPF; ResampleMovePF] in
  let all_models = [HMM; LinRegr] in
  
  Printf.printf "========== EXPERIMENT 1: Varying Dataset Size ==========\n\n";
  List.iter (fun variant ->
    List.iter (fun model ->
      Experiment1.run_experiment variant model
    ) all_models
  ) all_variants;
  
  Printf.printf "========== EXPERIMENT 2: Varying Particles ==========\n\n";
  List.iter (fun variant ->
    List.iter (fun model ->
      Experiment2.run_experiment variant model
    ) all_models
  ) all_variants;
  
  Printf.printf "=== All Benchmarks Complete ===\n"
