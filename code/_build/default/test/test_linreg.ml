open Code.Linreg

let mock_sample_observe : type a. (unit -> a) -> int list 
= fun f ->
  let open Effect.Deep in
  let observations = ref [] in
  let _ = match f () with
    | value -> value
    | effect (Sample ("m", _, _)), k ->
        continue k 2.0
    | effect (Sample ("c", _, _)), k ->
        continue k 1.0
    | effect (Observe ("y", _, _, sigma)), k ->
        observations := int_of_float sigma :: !observations;
        continue k ()
  in
  List.rev !observations

let test_linreg_model () =
  let data = 
    [ (1.0, 3.0); (2.0, 5.0); (3.0, 7.0); (4.0, 9.0); (5.0, 11.0) ] 
  in
  let results = mock_sample_observe (fun () -> lin_reg_model data) in
  Alcotest.(check (list int)) "observations collected" [3; 5; 7; 9; 11] results