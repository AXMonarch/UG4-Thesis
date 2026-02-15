(* Particle Filter Benchmarks for ppfx_modular                *)
(* Uses mtime to measure inference time in milliseconds       *)
(*                                                             *)
(* Benchmark 1: Varying particle count [50..500]              *)
(* Benchmark 2: Varying model size (data points) [100..500]   *)

open Effects
open Types
open Multpf

(* ---------------------------------------------------------- *)
(* Dynamic Linear Regression Model                            *)
(* Takes a list of (x, y) observations                        *)
(* ---------------------------------------------------------- *)

let fresh_addr (counter : int ref) (tag : string) : address =
  let local = !counter in
  incr counter;
  make_addr tag local

(* lin_regr_multi: linear regression with multiple data points *)
(* Matches ProbFX benchmark setup:                            *)
(*   - m ~ Normal(0, 3)                                       *)
(*   - c ~ Normal(0, 5)                                       *)
(*   - σ ~ Uniform(1, 3)                                      *)
(*   - y ~ Normal(m*x + c, σ) for each observation            *)
(* Returns: (m, c) — slope and intercept                      *)
let lin_regr_multi (xs : float list) (ys : float list) : (float * float) model =
  fun () ->
    let counter = ref 0 in
    (* Sample prior for slope and intercept *)
    let m = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "m"
              ; dist = Dist.Normal (0.0, 3.0) }) in
    let c = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "c"
              ; dist = Dist.Normal (0.0, 5.0) }) in
    let sigma = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "sigma"
              ; dist = Dist.Uniform (1.0, 3.0) }) in
    (* Observe each data point *)
    List.iter2 (fun x y ->
      Effect.perform (FloatEffects.Observe
                { addr = fresh_addr counter "obs"
                ; dist = Dist.Normal (m *. x +. c, sigma)
                ; obs  = y })
    ) xs ys;
    (m, c)

(* ---------------------------------------------------------- *)
(* Generate synthetic data: y = 3*x (matches ProbFX setup)   *)
(* ProbFX uses: xs = [0..n], ys = [3*x | x <- xs]            *)
(* ---------------------------------------------------------- *)
let generate_data (n : int) : float list * float list =
  let true_m = 3.0 in
  let true_c = 0.0 in
  (* Generate n+1 data points from 0 to n, matching ProbFX *)
  let xs = List.init (n + 1) (fun i -> float_of_int i) in
  let ys = List.map (fun x -> true_m *. x +. true_c) xs in
  (xs, ys)

(* ---------------------------------------------------------- *)
(* Timing utilities using mtime                               *)
(* ---------------------------------------------------------- *)
let time_ms (f : unit -> 'a) : float * 'a =
  let counter = Mtime_clock.counter () in
  let result = f () in
  let elapsed = Mtime_clock.count counter in
  let ms = Mtime.Span.to_float_ns elapsed /. 1_000_000.0 in
  (ms, result)

(* ---------------------------------------------------------- *)
(* Benchmark 1: Varying particle counts                       *)
(* Fixed model size, varying number of particles              *)
(* Matches: MPF-[ ]-LinRegr-50 from ProbFX benchmarks         *)
(* ---------------------------------------------------------- *)
let bench_varying_particles () =
  Printf.printf "\n=== Benchmark 1: Varying Particle Count ===\n";
  Printf.printf "Fixed model size: 50 data points\n";
  Printf.printf "True model: y = 3*x\n\n";
  
  let model_size = 50 in
  let (xs, ys) = generate_data model_size in
  let particle_counts = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  
  Printf.printf "%-12s %-15s %-10s %-10s\n"
    "Particles" "Time (ms)" "m_est" "c_est";
  Printf.printf "%s\n" (String.make 50 '-');
  
  List.iter (fun n_particles ->
    Random.init 42; (* Reset seed for reproducibility *)
    let model = lin_regr_multi xs ys in
    let (time_taken, results) = time_ms (fun () -> 
      mulpfilter n_particles model
    ) in
    
    (* Compute weighted mean estimate *)
    let log_ws = List.map snd results in
    let max_lw = List.fold_left max neg_infinity log_ws in
    let ws_norm = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
    let sum_w = List.fold_left ( +. ) 0.0 ws_norm in
    let ws_norm = List.map (fun w -> w /. sum_w) ws_norm in
    
    let m_est = List.fold_left2 
      (fun acc ((m, _), _) w -> acc +. m *. w) 
      0.0 results ws_norm in
    let c_est = List.fold_left2 
      (fun acc ((_, c), _) w -> acc +. c *. w) 
      0.0 results ws_norm in
    
    Printf.printf "%-12d %-15.2f %-10.4f %-10.4f\n"
      n_particles time_taken m_est c_est
  ) particle_counts;
  
  Printf.printf "\n"

(* ---------------------------------------------------------- *)
(* Benchmark 2: Varying model size (number of data points)    *)
(* Fixed particle count, varying number of observations       *)
(* Matches: LinRegr-[ ]-MPF-100 from ProbFX benchmarks        *)
(* ---------------------------------------------------------- *)
let bench_varying_model_size () =
  Printf.printf "\n=== Benchmark 2: Varying Model Size ===\n";
  Printf.printf "Fixed particle count: 100\n";
  Printf.printf "True model: y = 3*x\n\n";
  
  let n_particles = 100 in
  let model_sizes = [50; 100; 150; 200; 250; 300; 350; 400; 450; 500] in
  
  Printf.printf "%-12s %-15s %-10s %-10s\n"
    "Data Points" "Time (ms)" "m_est" "c_est";
  Printf.printf "%s\n" (String.make 50 '-');
  
  List.iter (fun model_size ->
    Random.init 42; (* Reset seed for reproducibility *)
    let (xs, ys) = generate_data model_size in
    let model = lin_regr_multi xs ys in
    let (time_taken, results) = time_ms (fun () -> 
      mulpfilter n_particles model
    ) in
    
    (* Compute weighted mean estimate *)
    let log_ws = List.map snd results in
    let max_lw = List.fold_left max neg_infinity log_ws in
    let ws_norm = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
    let sum_w = List.fold_left ( +. ) 0.0 ws_norm in
    let ws_norm = List.map (fun w -> w /. sum_w) ws_norm in
    
    let m_est = List.fold_left2 
      (fun acc ((m, _), _) w -> acc +. m *. w) 
      0.0 results ws_norm in
    let c_est = List.fold_left2 
      (fun acc ((_, c), _) w -> acc +. c *. w) 
      0.0 results ws_norm in
    
    Printf.printf "%-12d %-15.2f %-10.4f %-10.4f\n"
      model_size time_taken m_est c_est
  ) model_sizes;
  
  Printf.printf "\n"

(* ---------------------------------------------------------- *)
(* Main entry point                                           *)
(* ---------------------------------------------------------- *)
let () =
  Printf.printf "╔════════════════════════════════════════════════╗\n";
  Printf.printf "║  Particle Filter Performance Benchmarks        ║\n";
  Printf.printf "║  ppfx_modular - Linear Regression Model        ║\n";
  Printf.printf "╚════════════════════════════════════════════════╝\n";
  
  Random.init 42;
  bench_varying_particles ();
  bench_varying_model_size ();
  
  Printf.printf "Benchmarks completed.\n"
