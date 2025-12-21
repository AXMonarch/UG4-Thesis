open Bench_utils
open Models
open Pf.Multinomial_pf

let generate_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let bench_hmm_length seq_len num_particles =
  let obs = generate_observations seq_len in
  let program _step () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_multinomial_pf program num_particles seq_len 0.5 in
    ()

let run_benchmarks () =
  let num_repeats = 10 in
  let seq_lengths = [10; 20; 50; 100;] in
  let particle_counts = [50; 100;] in
  
  print_row ["Sequence_Length"; "Num_Particles"; "Avg_Time_ms"];
  
  List.iter (fun seq_len ->
    List.iter (fun num_particles ->
      let bench_fn = bench_hmm_length seq_len num_particles in
      let avg_time = repeat_time num_repeats bench_fn in
      print_row [
        string_of_int seq_len;
        string_of_int num_particles;
        string_of_float avg_time
      ]
    ) particle_counts
  ) seq_lengths

let () = run_benchmarks ()
