(* MPF Convergence Test - Compare with ProbFX                 *)
(* Shows particle values and weighted means to verify         *)
(* that the algorithm matches ProbFX's implementation         *)

open Effects
open Types
open Multpf
open Printf

(* ---------------------------------------------------------- *)
(* Linear Regression Model - Matching ProbFX exactly          *)
(* ---------------------------------------------------------- *)

let fresh_addr (counter : int ref) (tag : string) : address =
  let local = !counter in
  incr counter;
  make_addr tag local

let lin_regr_multi (xs : float list) (ys : float list) : (float * float) model =
  fun () ->
    let counter = ref 0 in
    let m = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "m"
              ; dist = Dist.Normal (0.0, 3.0) }) in
    let c = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "c"
              ; dist = Dist.Normal (0.0, 5.0) }) in
    let sigma = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "sigma"
              ; dist = Dist.Uniform (1.0, 3.0) }) in
    List.iter2 (fun x y ->
      Effect.perform (FloatEffects.Observe
                { addr = fresh_addr counter "obs"
                ; dist = Dist.Normal (m *. x +. c, sigma)
                ; obs  = y })
    ) xs ys;
    (m, c)

(* ---------------------------------------------------------- *)
(* Generate test data: y = 3*x (matching ProbFX)              *)
(* ---------------------------------------------------------- *)
let generate_data (n : int) : float list * float list =
  let xs = List.init (n + 1) (fun i -> float_of_int i) in
  let ys = List.map (fun x -> 3.0 *. x) xs in
  (xs, ys)

(* ---------------------------------------------------------- *)
(* Helper functions                                           *)
(* ---------------------------------------------------------- *)
let mean (xs : float list) : float =
  let sum = List.fold_left ( +. ) 0.0 xs in
  sum /. float_of_int (List.length xs)

let weighted_mean (values : float list) (weights : float list) : float =
  List.fold_left2 (fun acc v w -> acc +. v *. w) 0.0 values weights

let normalize_weights (log_ws : float list) : float list =
  let max_lw = List.fold_left max neg_infinity log_ws in
  let ws_norm = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
  let sum_w = List.fold_left ( +. ) 0.0 ws_norm in
  List.map (fun w -> w /. sum_w) ws_norm

(* ---------------------------------------------------------- *)
(* Test 1: Show individual particle values                    *)
(* ---------------------------------------------------------- *)
let test_particle_values () =
  Random.init 42;
  
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  MPF Particle Values - Linear Regression              ║\n";
  printf "║  True values: slope=3.0, intercept=0.0                ║\n";
  printf "║  Model: 10 data points, 20 particles                  ║\n";
  printf "╚════════════════════════════════════════════════════════╝\n\n";
  
  let (xs, ys) = generate_data 10 in
  let model = lin_regr_multi xs ys in
  let results = mulpfilter 20 model in
  
  let log_ws = List.map snd results in
  let weights = normalize_weights log_ws in
  
  printf "Particle  Slope(m)   Intercept(c)  Log Weight  Norm Weight\n";
  printf "--------  ---------  ------------  ----------  -----------\n";
  
  List.iteri (fun i ((m, c), lw) ->
    let w = List.nth weights i in
    printf "%8d  %9.6f  %12.6f  %10.4f  %11.8f\n" (i+1) m c lw w
  ) results;
  
  let ms = List.map (fun ((m, _), _) -> m) results in
  let cs = List.map (fun ((_, c), _) -> c) results in
  
  printf "\n--- Unweighted Statistics ---\n";
  printf "Mean slope:     %.6f\n" (mean ms);
  printf "Mean intercept: %.6f\n" (mean cs);
  
  printf "\n--- Weighted Statistics (Proper Posterior) ---\n";
  printf "Mean slope:     %.6f\n" (weighted_mean ms weights);
  printf "Mean intercept: %.6f\n" (weighted_mean cs weights);
  
  printf "\nError from true values:\n";
  printf "Slope error:     %.6f\n" (abs_float (weighted_mean ms weights -. 3.0));
  printf "Intercept error: %.6f\n" (abs_float (weighted_mean cs weights -. 0.0));
  printf "\n"

(* ---------------------------------------------------------- *)
(* Test 2: Convergence with increasing particle count         *)
(* ---------------------------------------------------------- *)
let test_convergence_particles () =
  Random.init 42;
  
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  MPF Convergence Test - Varying Particle Count        ║\n";
  printf "║  True values: slope=3.0, intercept=0.0                ║\n";
  printf "║  Model: 10 data points                                ║\n";
  printf "╚════════════════════════════════════════════════════════╝\n\n";
  
  let (xs, ys) = generate_data 10 in
  let particle_counts = [10; 20; 50; 100; 200; 500] in
  
  printf "Particles  Mean Slope (weighted)  Mean Intercept  Slope Error  Int Error\n";
  printf "---------  --------------------  --------------  -----------  ---------\n";
  
  List.iter (fun n_particles ->
    Random.init 42; (* Same seed for fair comparison *)
    let model = lin_regr_multi xs ys in
    let results = mulpfilter n_particles model in
    
    let log_ws = List.map snd results in
    let weights = normalize_weights log_ws in
    let ms = List.map (fun ((m, _), _) -> m) results in
    let cs = List.map (fun ((_, c), _) -> c) results in
    
    let mean_m = weighted_mean ms weights in
    let mean_c = weighted_mean cs weights in
    let error_m = abs_float (mean_m -. 3.0) in
    let error_c = abs_float (mean_c -. 0.0) in
    
    printf "%9d  %20.6f  %14.6f  %11.6f  %9.6f\n"
      n_particles mean_m mean_c error_m error_c
  ) particle_counts;
  
  printf "\n✓ As particle count increases, estimates should converge to true values\n";
  printf "\n"

(* ---------------------------------------------------------- *)
(* Test 3: Compare different random seeds                     *)
(* ---------------------------------------------------------- *)
let test_stability () =
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  MPF Stability Test - Multiple Random Seeds           ║\n";
  printf "║  Model: 10 data points, 100 particles                 ║\n";
  printf "╚════════════════════════════════════════════════════════╝\n\n";
  
  let (xs, ys) = generate_data 10 in
  let seeds = [42; 123; 456; 789; 1337] in
  
  printf "Seed  Mean Slope  Mean Intercept  Slope Error  Int Error\n";
  printf "----  ----------  --------------  -----------  ---------\n";
  
  List.iter (fun seed ->
    Random.init seed;
    let model = lin_regr_multi xs ys in
    let results = mulpfilter 100 model in
    
    let log_ws = List.map snd results in
    let weights = normalize_weights log_ws in
    let ms = List.map (fun ((m, _), _) -> m) results in
    let cs = List.map (fun ((_, c), _) -> c) results in
    
    let mean_m = weighted_mean ms weights in
    let mean_c = weighted_mean cs weights in
    let error_m = abs_float (mean_m -. 3.0) in
    let error_c = abs_float (mean_c -. 0.0) in
    
    printf "%4d  %10.6f  %14.6f  %11.6f  %9.6f\n"
      seed mean_m mean_c error_m error_c
  ) seeds;
  
  printf "\n✓ Results should be stable across different random seeds\n";
  printf "\n"

(* ---------------------------------------------------------- *)
(* Main entry point                                           *)
(* ---------------------------------------------------------- *)
let () =
  test_particle_values ();
  test_convergence_particles ();
  test_stability ();
  printf "All MPF convergence tests completed.\n"
