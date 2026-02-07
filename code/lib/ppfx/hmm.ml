open Effects

let fixed_hmm = 51

let hmm_model () =
  let trans_p =
    Effect.perform (Sample {
      addr = make_addr "trans_p" 0;
      dist = Beta (2.0, 2.0);
    })
  in
  let obs_p =
    Effect.perform (Sample {
      addr = make_addr "obs_p" 0;
      dist = Beta (2.0, 2.0);
    })
  in

  let x = ref 0 in

  let observed_y = Array.init fixed_hmm (fun _ -> 0) in

  for t = 0 to fixed_hmm - 1 do
    let dx =
      Effect.perform (Sample {
        addr = make_addr "dx" t;
        dist = Bernoulli trans_p;
      }) in
    let dx_bool = dx > 0.5 in
    x := !x + (if dx_bool then 1 else 0);

    let y_obs = observed_y.(t) in
    Effect.perform (Observe {
    addr = make_addr "y" t;
    dist = Binomial (!x, obs_p);
    obs  = float_of_int y_obs;
  })
  done;

  (!x, trans_p, obs_p)