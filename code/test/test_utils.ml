open Code.Utils

let test_mean_basic () =
  let data = 
    [ 1.0; 1.0; 1.0 ] 
  in
  Alcotest.(check (float 0.000001)) "Mean : " 1.0 (mean data)

let test_mean_five () =
  let data =
    [ 1.0; 2.0; 3.0; 4.0; 5.0 ]
  in
  Alcotest.(check (float 0.000001)) "Mean" 3.0 (mean data)

