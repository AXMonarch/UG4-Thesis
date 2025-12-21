open Bench_utils
open Models
open Mh.Mh_base

let generate_observations n =
  Array.init n (fun _ -> Random.float 10.0)

let simple_propose_fn _name _dist current_val =
  let step_size = 0.5 in
  let step = (Random.float (2.0 *. step_size)) -. step_size in
  Some (current_val +. step)

let bench_hmm_length seq_len num_mh_iters =
  let obs = generate_observations seq_len in
  let program () = Hmm.hidden_markov_model 5 obs in
  fun () ->
    let _ = run_mh program num_mh_iters simple_propose_fn in
    ()

let run_benchmarks () =
  let num_mh_iters = 100 in
  let num_repeats = 10 in
  let seq_lengths = [10; 20; 50; 100;] in
  
  print_row ["Sequence_Length"; "Avg_Time_ms"; "MH_Iterations"];
  
  List.iter (fun seq_len ->
    let bench_fn = bench_hmm_length seq_len num_mh_iters in
    let avg_time = repeat_time num_repeats bench_fn in
    print_row [
      string_of_int seq_len;
      string_of_float avg_time;
      string_of_int num_mh_iters
    ]
  ) seq_lengths

let () = run_benchmarks ()
