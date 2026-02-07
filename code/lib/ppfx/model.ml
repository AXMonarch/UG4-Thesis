open Effects

let simple_model () =
  let w =
    Effect.perform (Sample {
      addr = make_addr "w" 0;
      dist = Normal (0.0, 1.0);
    })
  in

  Effect.perform (Observe {
    addr = make_addr "y" 0;
    dist = Normal (w, 0.5);
    obs  = 1.0;
  });

  w