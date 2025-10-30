open Effect
open Effects

let linear_regression (data : Types.dataset) : float * float =
  let m = perform (Sample { name = "m"; dist = Uniform (-10.0, 10.0) }) in
  let c = perform (Sample { name = "c"; dist = Uniform (-10.0, 10.0) }) in
  List.iteri
    (fun i (x, y) ->
      let pred = (m *. x) +. c in
      perform
        (Observe
           {
             name = "y_" ^ string_of_int i;
             dist = Normal (pred, 1.0);
             obs = y;
           }))
    data;
  (m, c)

let log_likelihood (data : Types.dataset) (m, c) : float =
  List.fold_left
    (fun acc (x, y) ->
      let pred = (m *. x) +. c in
      let diff = y -. pred in
      acc -. (0.5 *. diff *. diff))
    0.0 data
