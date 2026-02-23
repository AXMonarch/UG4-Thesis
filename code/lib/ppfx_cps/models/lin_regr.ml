open Effects
let addr_m   = Addr.make ()
let addr_c   = Addr.make ()
let addr_obs = Addr.make ()

let lin_regr (x : float) (y : float)
    : (float sample_cap * float observe_cap, float * float) model
  = Model (fun ((module S), (module O)) ->
      let m = Effect.perform (S.Sample (Dist.normal 0. 3., addr_m)) in
      let c = Effect.perform (S.Sample (Dist.normal 0. 2., addr_c)) in
      ignore (Effect.perform (O.Observe (Dist.normal (m *. x +. c) 1., addr_obs, y)));
      (m, c))