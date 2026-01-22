(*

  Experiment 1: Varying Dataset Size (Fixed Algorithm Parameters)
 
  Experiment 2: Varying Algorithm Parameters (Fixed Dataset Size)

*)

open Bench_utils
open Models
open Mh.Single_site_mh
open Mh.Particle_mh

type mh_variant = SingleSiteMH | ParticleMH
type model_type = HMM | LinRegr

(* Data generation functions *)
let generate_hmm_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let generate_linregr_data n =
  List.init n (fun i ->
    let x = float_of_int i in
    let y = 2.0 *. x +. 3.0 +. (Random.float 2.0 -. 1.0) in
    (x, y))

(* Configuration *)
let num_repeats = 10

(* ========== EXPERIMENT 1: Varying Dataset Size ========== *)
module Experiment1 = struct
  let fixed_iterations = 100
  let fixed_step_size = 0.5
  let fixed_pmh_particles = 10
  let dataset_sizes = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500]

  (* Benchmark functions for each variant *)
  let bench_single_site_mh model_type size =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_single_site_mh program fixed_iterations fixed_step_size in
          ()
    | LinRegr ->
        let data = generate_linregr_data size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_single_site_mh program fixed_iterations fixed_step_size in
          ()

  let bench_particle_mh model_type size =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_particle_mh program fixed_iterations fixed_pmh_particles fixed_step_size in
          ()
    | LinRegr ->
        let data = generate_linregr_data size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_particle_mh program fixed_iterations fixed_pmh_particles fixed_step_size in
          ()

  let bench_variant variant model_type size =
    match variant with
    | SingleSiteMH -> bench_single_site_mh model_type size
    | ParticleMH -> bench_particle_mh model_type size

  let variant_to_string = function
    | SingleSiteMH -> "SingleSiteMH"
    | ParticleMH -> "ParticleMH"

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
    
    print_to_file ["Dataset_Size"; "Avg_Time_ms"; "Fixed_Iterations"];
    
    List.iter (fun size ->
      let bench_fn = bench_variant variant model_type size in
      let avg_time = repeat_time num_repeats bench_fn in
      print_to_file [
        string_of_int size;
        Printf.sprintf "%.2f" avg_time;
        string_of_int fixed_iterations
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
  let fixed_step_size = 0.5
  let fixed_pmh_particles = 10
  let iteration_counts = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000]

  let bench_single_site_mh model_type num_iters =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations fixed_hmm_size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_single_site_mh program num_iters fixed_step_size in
          ()
    | LinRegr ->
        let data = generate_linregr_data fixed_linregr_size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_single_site_mh program num_iters fixed_step_size in
          ()

  let bench_particle_mh model_type num_iters =
    match model_type with
    | HMM ->
        let obs = generate_hmm_observations fixed_hmm_size in
        let program () = Hmm.hidden_markov_model 5 obs in
        fun () ->
          let _ = run_particle_mh program num_iters fixed_pmh_particles fixed_step_size in
          ()
    | LinRegr ->
        let data = generate_linregr_data fixed_linregr_size in
        let program () = Linear_regression.linear_regression data in
        fun () ->
          let _ = run_particle_mh program num_iters fixed_pmh_particles fixed_step_size in
          ()

  let bench_variant variant model_type num_iters =
    match variant with
    | SingleSiteMH -> bench_single_site_mh model_type num_iters
    | ParticleMH -> bench_particle_mh model_type num_iters

  let variant_to_string = function
    | SingleSiteMH -> "SingleSiteMH"
    | ParticleMH -> "ParticleMH"

  let model_to_string = function
    | HMM -> "HMM"
    | LinRegr -> "LinRegr"

  let run_experiment variant model_type =
    let variant_name = variant_to_string variant in
    let model_name = model_to_string model_type in
    let filename = Printf.sprintf "varying_iterations_%s_%s.csv" 
      (String.lowercase_ascii variant_name) 
      (String.lowercase_ascii model_name) in
    
    Printf.printf "Running Experiment 2 (%s on %s)...\n" variant_name model_name;
    
    let oc = open_out ("benchmark_results/" ^ filename) in
    let print_to_file row =
      output_string oc (String.concat "," row ^ "\n")
    in
    
    print_to_file ["Num_Iterations"; "Avg_Time_ms"; "Fixed_Dataset_Size"];
    
    List.iter (fun num_iters ->
      let bench_fn = bench_variant variant model_type num_iters in
      let avg_time = repeat_time num_repeats bench_fn in
      let fixed_size = match model_type with
        | HMM -> fixed_hmm_size
        | LinRegr -> fixed_linregr_size
      in
      print_to_file [
        string_of_int num_iters;
        Printf.sprintf "%.2f" avg_time;
        string_of_int fixed_size
      ];
      Printf.printf "  Iterations %d: %.2f ms\n" num_iters avg_time;
      flush stdout
    ) iteration_counts;
    
    close_out oc;
    Printf.printf "Results saved to benchmark_results/%s\n\n" filename
end


let () =
  Random.init 42;
  Printf.printf "\n=== MH Variants Benchmark Suite ===\n\n";
  
  let all_variants = [SingleSiteMH; ParticleMH] in
  let all_models = [HMM; LinRegr] in
  
  Printf.printf "========== EXPERIMENT 1: Varying Dataset Size ==========\n\n";
  List.iter (fun variant ->
    List.iter (fun model ->
      Experiment1.run_experiment variant model
    ) all_models
  ) all_variants;
  
  Printf.printf "========== EXPERIMENT 2: Varying Iterations ==========\n\n";
  List.iter (fun variant ->
    List.iter (fun model ->
      Experiment2.run_experiment variant model
    ) all_models
  ) all_variants;
  
  Printf.printf "=== All Benchmarks Complete ===\n"
