[@@@ocaml.warning "-27-33-39"]

open Effect
open Effect.Deep
open Effects
open Models

type particle = {
  trace : trace;
  mutable log_weight : float;
}

type particle_cloud = particle array

let empty_particle () =
  { trace = { choices = Hashtbl.create 16; log_prob = 0.0 };
    log_weight = 0.0 }

let sample_from_dist = function
  | Uniform (a, b) ->
      a +. Random.float (b -. a)
  | Normal (mu, sigma) ->
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      let z =
        sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)
      in
      mu +. sigma *. z

let log_pdf dist value =
  match dist with
  | Uniform (a, b) ->
      if value >= a && value <= b
      then -.log (b -. a)
      else neg_infinity
  | Normal (mu, sigma) ->
      let diff = value -. mu in
      -.0.5 *. log (2.0 *. Float.pi *. sigma *. sigma)
      -. (diff *. diff) /. (2.0 *. sigma *. sigma)

let normalize_log_weights particles =
  let max_logw =
    Array.fold_left (fun acc p -> max acc p.log_weight)
      neg_infinity particles
  in
  let weights =
    Array.map (fun p -> exp (p.log_weight -. max_logw)) particles
  in
  let z = Array.fold_left (+.) 0.0 weights in
  Array.mapi (fun i p ->
    (p, weights.(i) /. z)
  ) particles

let effective_sample_size weights =
  let sum_sq =
    Array.fold_left (fun acc (_, w) -> acc +. w *. w) 0.0 weights
  in
  1.0 /. sum_sq

let resample particles weights =
  let n = Array.length particles in
  let cdf = Array.make n 0.0 in
  let _ =
    Array.fold_left
      (fun acc (i, (_, w)) ->
        let new_acc = acc +. w in
        cdf.(i) <- new_acc;
        new_acc
      ) 0.0 (Array.mapi (fun i pw -> (i, pw)) weights)
  in
  Array.init n (fun _ ->
    let r = Random.float 1.0 in
    let rec pick i =
      if r <= cdf.(i) then
        let p = fst weights.(i) in
        { trace =
            { choices = Hashtbl.copy p.trace.choices;
              log_prob = p.trace.log_prob };
          log_weight = 0.0 }
      else pick (i + 1)
    in
    pick 0
  )

let pf_handler
    ~(particles : int)
    ~(resample_threshold : float)
    (model : unit -> 'a)
  : particle_cloud =

  let cloud = Array.init particles (fun _ -> empty_particle ()) in

  let rec handle () =
    match model () with
    | _ -> cloud

    | effect (Sample { name; dist }), k ->
        Array.iter (fun p ->
          let v =
            match Hashtbl.find_opt p.trace.choices name with
            | Some v -> v
            | None ->
                let v = sample_from_dist dist in
                Hashtbl.add p.trace.choices name v;
                v
          in
          p.trace.log_prob <- p.trace.log_prob +. log_pdf dist v
        ) cloud;
        continue k (Hashtbl.find cloud.(0).trace.choices name)

    | effect (Observe { dist; obs; _ }), k ->
        Array.iter (fun p ->
          let lp = log_pdf dist obs in
          p.trace.log_prob <- p.trace.log_prob +. lp;
          p.log_weight <- p.log_weight +. lp
        ) cloud;

        let weighted = normalize_log_weights cloud in
        let ess = effective_sample_size weighted in
        let n = float_of_int particles in

        let new_cloud =
          if ess < resample_threshold *. n then
            resample cloud weighted
          else
            cloud
        in

        Array.iteri (fun i p -> cloud.(i) <- p) new_cloud;
        continue k ()
  in

  handle ()