(** Main entry point demonstrating probabilistic models and inference *)

open Models.Linear_regression
open Models.Types
open Utils.Stats
open Mh.Mh_base
open Mh.Independence_mh

let () =
  let open Printf in
  Random.self_init ();
  
  (* Create a simple linear dataset: y = 2x + 1 *)
  let dataset : dataset = [ 
    (1.0, 3.0); (2.0, 5.0); (3.0, 7.0); (4.0, 9.0); (5.0, 11.0) 
  ] in
  
  printf "=== Linear Regression with Metropolis-Hastings ===\n\n";
  
  (* Run the model once to demonstrate effect handling *)
  printf "Running model with MH handler (verbose):\n";
  let _ = mh_handler (fun () -> linear_regression dataset) in
  
  printf "\n--- Running MCMC chain ---\n";
  
  (* Run Independence MH *)
  let samples = independence_mh_chain 
    (likelihood dataset) 
    propose_independent 
    100 
    (0.0, 0.0) 
    [] 
  in
  
  let ms = List.map fst samples and cs = List.map snd samples in
  
  printf "\n=== Results ===\n";
  printf "Posterior mean m ≈ %.3f (true value: 2.0)\n" (mean ms);
  printf "Posterior std m ≈ %.3f\n" (std_dev ms);
  printf "Posterior mean c ≈ %.3f (true value: 1.0)\n" (mean cs);
  printf "Posterior std c ≈ %.3f\n" (std_dev cs);
  
  printf "\n✓ Linear regression inference complete!\n"
