(*
  Run all benchmarks matching ProbFX experiments structure
  
  This script runs comprehensive benchmarks for both MH and PF variants
  across different models to match the ProbFX benchmark structure.
*)

open Bench_utils
open Models
open Mh.Single_site_mh
open Mh.Particle_mh
open Pf.Multinomial_pf
open Pf.Resample_move_pf

let generate_hmm_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let generate_linregr_data n =
  List.init n (fun i ->
    let x = float_of_int i in
    let y = 2.0 *. x +. 3.0 +. (Random.float 2.0 -. 1.0) in
    (x, y))

let num_repeats = 10

(* ========== EXPERIMENT 1: Varying Model Size (Fixed Algorithm) ========== *)

(* LinRegr with varying datapoints *)
let bench_linregr_ssmh_100 size =
  let data = generate_linregr_data size in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_single_site_mh program 100 0.5 in
    ()

let bench_linregr_mpf_100 size =
  let data = generate_linregr_data size in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_multinomial_pf program 100 0.5 in
    ()

let bench_linregr_pmh_50_10 size =
  let data = generate_linregr_data size in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_particle_mh program 10 50 0.5 in
    ()

let bench_linregr_rmpf_10_1 size =
  let data = generate_linregr_data size in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_resample_move_pf program 10 1 0.5 0.5 in
    ()

(* HMM with varying nodes *)
let bench_hmm_ssmh_100 size =
  let obs = generate_hmm_observations size in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_single_site_mh program 100 0.5 in
    ()

let bench_hmm_mpf_100 size =
  let obs = generate_hmm_observations size in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_multinomial_pf program 100 0.5 in
    ()

let bench_hmm_pmh_50_10 size =
  let obs = generate_hmm_observations size in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_particle_mh program 10 50 0.5 in
    ()

let bench_hmm_rmpf_10_1 size =
  let obs = generate_hmm_observations size in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_resample_move_pf program 10 1 0.5 0.5 in
    ()

(* ========== EXPERIMENT 2: Varying Algorithm Params (Fixed Model Size) ========== *)

(* Varying SSMH steps *)
let bench_ssmh_linregr_50 steps =
  let data = generate_linregr_data 50 in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_single_site_mh program steps 0.5 in
    ()

let bench_ssmh_hmm_50 steps =
  let obs = generate_hmm_observations 50 in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_single_site_mh program steps 0.5 in
    ()

(* Varying MPF particles *)
let bench_mpf_linregr_50 particles =
  let data = generate_linregr_data 50 in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_multinomial_pf program particles 0.5 in
    ()

let bench_mpf_hmm_50 particles =
  let obs = generate_hmm_observations 50 in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_multinomial_pf program particles 0.5 in
    ()

let bench_pmh_linregr_50 particles =
  let data = generate_linregr_data 50 in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_particle_mh program 50 particles 0.5 in
    ()

let bench_pmh_hmm_50 particles =
  let obs = generate_hmm_observations 50 in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_particle_mh program 50 particles 0.5 in
    ()

(* Varying RMPF MH steps *)
let bench_rmpf_linregr_50 mh_steps =
  let data = generate_linregr_data 50 in
  let program () = Linear_regression.linear_regression data in
  fun () ->
    let _ = run_resample_move_pf program 10 mh_steps 0.5 0.5 in
    ()

let bench_rmpf_hmm_50 mh_steps =
  let obs = generate_hmm_observations 50 in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_resample_move_pf program 10 mh_steps 0.5 0.5 in
    ()

(* ========== Main Execution ========== *)

let () =
  Random.self_init ();
  
  let oc = open_out "benchmarks_ocaml_fused.csv" in
  let printf fmt = Printf.fprintf oc fmt in
  
  (* Header for varying model size experiments *)
  printf "Num datapoints,50,100,150,200,250,300,350,400,450,500\n";
  
  (* LinRegr experiments *)
  let sizes = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  
  (* SSMH-100 *)
  printf "LinRegr-[ ]-SSMH-100";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_linregr_ssmh_100 size) in
    printf ",%.15e" (time /. 1000.0)  (* Convert to seconds *)
  ) sizes;
  printf "\n";
  
  (* MPF-100 *)
  printf "LinRegr-[ ]-MPF-100";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_linregr_mpf_100 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* PMH-50-10 *)
  printf "LinRegr-[ ]-PMH-50-10";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_linregr_pmh_50_10 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* RMPF-10-1 *)
  printf "LinRegr-[ ]-RMPF-10-1";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_linregr_rmpf_10_1 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* HMM experiments *)
  printf "Num nodes,50,100,150,200,250,300,350,400,450,500\n";
  
  (* SSMH-100 *)
  printf "HidMark-[ ]-SSMH-100";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_hmm_ssmh_100 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* MPF-100 *)
  printf "HidMark-[ ]-MPF-100";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_hmm_mpf_100 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* PMH-50-10 *)
  printf "HidMark-[ ]-PMH-50-10";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_hmm_pmh_50_10 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* RMPF-10-1 *)
  printf "HidMark-[ ]-RMPF-10-1";
  List.iter (fun size ->
    let time = repeat_time num_repeats (bench_hmm_rmpf_10_1 size) in
    printf ",%.15e" (time /. 1000.0)
  ) sizes;
  printf "\n";
  
  (* Varying algorithm parameters experiments *)
  
  (* SSMH steps *)
  printf "Num SSMH steps,100,200,300,400,500,600,700,800,900,1000\n";
  let steps_range = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
  
  printf "SSMH-[ ]-LinRegr-50";
  List.iter (fun steps ->
    let time = repeat_time num_repeats (bench_ssmh_linregr_50 steps) in
    printf ",%.15e" (time /. 1000.0)
  ) steps_range;
  printf "\n";
  
  printf "SSMH-[ ]-HidMark-50";
  List.iter (fun steps ->
    let time = repeat_time num_repeats (bench_ssmh_hmm_50 steps) in
    printf ",%.15e" (time /. 1000.0)
  ) steps_range;
  printf "\n";
  
  (* MPF particles *)
  printf "Num MPF particles,50,100,150,200,250,300,350,400,450,500\n";
  let particles_range = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  
  printf "MPF-[ ]-LinRegr-50";
  List.iter (fun particles ->
    let time = repeat_time num_repeats (bench_mpf_linregr_50 particles) in
    printf ",%.15e" (time /. 1000.0)
  ) particles_range;
  printf "\n";
  
  printf "MPF-[ ]-HidMark-50";
  List.iter (fun particles ->
    let time = repeat_time num_repeats (bench_mpf_hmm_50 particles) in
    printf ",%.15e" (time /. 1000.0)
  ) particles_range;
  printf "\n";
  
  (* PMH particles *)
  printf "Num PMH particles,10,20,30,40,50,60,70,80,90,100\n";
  let pmh_particles_range = [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  
  printf "PMH-50-[ ]-LinRegr-50";
  List.iter (fun particles ->
    let time = repeat_time num_repeats (bench_pmh_linregr_50 particles) in
    printf ",%.15e" (time /. 1000.0)
  ) pmh_particles_range;
  printf "\n";
  
  printf "PMH-50-[ ]-HidMark-50";
  List.iter (fun particles ->
    let time = repeat_time num_repeats (bench_pmh_hmm_50 particles) in
    printf ",%.15e" (time /. 1000.0)
  ) pmh_particles_range;
  printf "\n";
  
  (* RMPF MH steps *)
  printf "Num RMPF mh steps,10,20,30,40,50,60,70,80,90,100\n";
  let rmpf_steps_range = [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
  
  printf "RMPF-10-[ ]-LinRegr-50";
  List.iter (fun mh_steps ->
    let time = repeat_time num_repeats (bench_rmpf_linregr_50 mh_steps) in
    printf ",%.15e" (time /. 1000.0)
  ) rmpf_steps_range;
  printf "\n";
  
  printf "RMPF-10-[ ]-HidMark-50";
  List.iter (fun mh_steps ->
    let time = repeat_time num_repeats (bench_rmpf_hmm_50 mh_steps) in
    printf ",%.15e" (time /. 1000.0)
  ) rmpf_steps_range;
  printf "\n";
  
  close_out oc;
  print_endline "Benchmarks complete! Results saved to benchmarks_ocaml_fused.csv"
