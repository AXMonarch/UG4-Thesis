open Effects

(* Fixed dataset size for ProbFX comparison *)
let fixed_lr = 50

(* Simple 1D linear regression with 50 data points: y = 2*x + 1 + small noise *)
let linreg_model () =
  (* latent parameters *)
  let w =
    Effect.perform (Sample { addr = make_addr "w" 0; dist = Normal (0.0, 1.0) })
  in
  let b =
    Effect.perform (Sample { addr = make_addr "b" 0; dist = Normal (0.0, 1.0) })
  in

  (* fixed dataset: 50 points, y = 2*x + 1 *)
  let x_data = List.init fixed_lr (fun i -> float i) in
  let y_data = List.map (fun x -> 2.0 *. x +. 1.0) x_data in

  (* Observe each data point *)
  List.iteri (fun i x ->
    let y_obs = List.nth y_data i in
    let mean = w *. x +. b in
    Effect.perform (Observe {
      addr = make_addr ("y" ^ string_of_int i) 0;
      dist = Normal (mean, 0.1);  (* small noise to match ProbFX *)
      obs  = y_obs;
    })
  ) x_data;

  (* return latent parameters *)
  (w, b)