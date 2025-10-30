open Alcotest
open Models
open Effect.Deep

let rec handle f =
  match f () with
  | effect (Effects.Sample s), k ->
      let value =
        match s.dist with
        | Effects.Uniform (low, high) -> (low +. high) /. 2.0
        | Effects.Normal (mu, _) -> mu
      in
      handle (fun () -> continue k value)
  | effect (Effects.Observe _), k ->
      handle (fun () -> continue k ())
  | x -> x

let test_linear_regression () =
  let data = [ (1.0, 2.0); (2.0, 3.0) ] in
  let m, c = handle (fun () -> Linear_regression.linear_regression data) in
  check (float 0.0) "m" 0.0 m;
  check (float 0.0) "c" 0.0 c

let test_hmm () =
  let observations = [| 1.0; 2.0; 3.0 |] in
  let states = handle (fun () -> Hmm.hidden_markov_model 3 observations) in
  check (list int) "states" [ 1; 1; 1 ] states

let suite = [
  ( "linear_regression",
    [ test_case "linear_regression" `Quick test_linear_regression ] );
  ("hmm", [ test_case "hmm" `Quick test_hmm ]);
]