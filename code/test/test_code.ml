(** Main test runner *)

let () =
  Alcotest.run "Effect Handlers Tests" (
    Test_utils.suite @ Test_models.suite @ Test_inference.suite
  )
