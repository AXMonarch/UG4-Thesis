open Test_linreg
open Test_utils

  let () =
    let open Alcotest in
    run "Utils tests" [
      ("Mean", 
        [ test_case "One" `Quick test_mean_basic 
        ; test_case "Five" `Quick test_mean_five
        ] );
      ("Mock sample&observe", 
        [ test_case "lin_reg_model" `Quick test_linreg_model ] );
    ]