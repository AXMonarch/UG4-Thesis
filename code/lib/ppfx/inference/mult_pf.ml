open Effects
open Runner

let sum_lp (lp : lp_trace) : float =
  AddrMap.fold (fun _ v acc -> acc +. v) lp 0.0

let normalize_weights (weights : float array) : float array =
  let total = Array.fold_left (+.) 0.0 weights in
  Array.map (fun w -> w /. total) weights


let handle_resample thunk =
  let handler t =
    match t () with
    | v -> v
    | effect (Resample weights), k ->
        let total = Array.fold_left (+.) 0.0 weights in
        let u = Random.float total in
        let rec find_index i acc =
          if i >= Array.length weights then Array.length weights - 1
          else if u <= acc +. weights.(i) then i
          else find_index (i + 1) (acc +. weights.(i))
        in
        let idx = find_index 0 0.0 in
        Effect.Deep.continue k idx
    | effect e, k -> Effect.Deep.continue k (Obj.magic e)
  in
  handler thunk

let resample_particles (particles : 'a particle array) : 'a particle array =
  let weights = Array.map (fun p -> p.weight) particles in
  let norm_weights = normalize_weights weights in
  Array.init (Array.length particles) (fun _ ->
      let idx = Effect.perform (Resample norm_weights) in
      particles.(idx)
  )

let propagate_particle (p : 'a particle) : 'a particle =
  let (res, lp, tr) = run_with_trace p.suspended_model (Some p.trace) in
  { trace = tr;
    weight = exp (sum_lp lp);
    suspended_model = (fun () -> res) }

let mult_pf
    ~(n_particles : int)
    (model : unit -> 'a)
  : 'a particle array =

  handle_resample (fun () ->
    let particles =
      Array.init n_particles (fun _ ->
        let (res, lp, tr) = run_with_trace model None in
        { trace = tr;
          weight = exp (sum_lp lp);
          suspended_model = (fun () -> res) }
      )
    in

    resample_particles particles
  )