(* test_utils.ml *)

open Utils.Stats
open Alcotest

let () = Random.self_init ()

let test_mean_basic () =
  let data = [ 1.0; 1.0; 1.0 ] in
  check (float 0.000001) "Mean of [1.0; 1.0; 1.0]" 1.0 (mean data)

let test_mean_five () =
  let data = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] in
  check (float 0.000001) "Mean of [1.0; 2.0; 3.0; 4.0; 5.0]" 3.0 (mean data)

let test_variance () =
  let data = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] in
  let expected = 2.0 in  (* variance of 1..5 is 2.0 *)
  check (float 0.000001) "Variance" expected (variance data)

let test_std_dev () =
  let data = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] in
  let expected = sqrt 2.0 in
  check (float 0.000001) "Std dev" expected (std_dev data)

let test_normalize_weights () =
  let weights = [| 1.0; 2.0; 3.0; 4.0 |] in
  let normalized = normalize_weights weights in
  let sum = Array.fold_left ( +. ) 0.0 normalized in
  check (float 0.000001) "Normalized weights sum to 1" 1.0 sum

let suite = [
  ("statistics", [
    test_case "mean basic" `Quick test_mean_basic;
    test_case "mean five" `Quick test_mean_five;
    test_case "variance" `Quick test_variance;
    test_case "std_dev" `Quick test_std_dev;
    test_case "normalize_weights" `Quick test_normalize_weights;
  ]);
]