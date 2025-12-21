open Bench_utils
open Models
open Pf.Pf_base

let generate_observations n =
  Array.init n (fun _ -> Random.float 10.0)

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

let bench_hmm_length seq_len num_particles =
  let obs = generate_observations seq_len in
  let program _step () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_pf program num_particles seq_len simple_resample 0.5 in
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
