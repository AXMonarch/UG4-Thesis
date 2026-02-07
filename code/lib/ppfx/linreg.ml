open Effects

let fixed_lr = 51

let linreg_model () =
  let m =
    Effect.perform (Sample {
      addr = make_addr "m" 0;
      dist = Normal (0.0, 3.0)
    })
  in
  let c =
    Effect.perform (Sample {
      addr = make_addr "c" 0;
      dist = Normal (0.0, 5.0)
    })
  in
  let sigma =
    Effect.perform (Sample {
      addr = make_addr "sigma" 0;
      dist = Uniform (1.0, 3.0)
    })
  in

  let x_data = List.init (fixed_lr + 1) (fun i -> float i) in

  List.iteri (fun i x ->
    let y_obs = 3.0 *. x in
    Effect.perform (Observe {
      addr = make_addr ("y" ^ string_of_int i) 0;
      dist = Normal (m *. x +. c, sigma);
      obs  = y_obs;
    })
  ) x_data;

  (m, c, sigma)