open Mh
open Pf

(* ---- models (same as mh_test) ---- *)

let lin_regr_full (xs : float list) (ys : float list)
    : < sample  : 'a. 'a Dist.t -> 'a;
        observe : 'b. 'b Dist.t -> 'b -> unit; .. >
    -> float * float
  = fun cap ->
    let m = cap#sample (Dist.normal 0. 3.) in
    let c = cap#sample (Dist.normal 0. 2.) in
    List.iter2 (fun x y ->
      cap#observe (Dist.normal (m *. x +. c) 1.) y
    ) xs ys;
    (m, c)

let hmm (n : int) (x0 : int) (ys : int array)
    : < sample  : 'a. 'a Dist.t -> 'a;
        observe : 'b. 'b Dist.t -> 'b -> unit; .. >
    -> float * float
  = fun cap ->
    let trans_p = cap#sample (Dist.beta 2. 2.) in
    let obs_p   = cap#sample (Dist.beta 2. 2.) in
    let x       = ref x0 in
    for i = 0 to n - 1 do
      let dx = if cap#sample (Dist.bernoulli trans_p) then 1 else 0 in
      x := !x + dx;
      cap#observe (Dist.binomial !x obs_p) ys.(i)
    done;
    (trans_p, obs_p)

let simulate_hmm (n : int) (x0 : int) (trans_p : float) (obs_p : float) : int array =
  let x  = ref x0 in
  let ys = Array.make n 0 in
  for i = 0 to n - 1 do
    let dx = if Random.float 1.0 <= trans_p then 1 else 0 in
    x := !x + dx;
    ys.(i) <- Dist.draw (Random.float 1.0) (Dist.binomial !x obs_p)
  done;
  ys

(* ---- helpers ---- *)

(* extract (m, c) posterior mean from a particle list *)
let pf_posterior_mean (particles : (float * float) particle list) =
  let finished = List.filter_map (fun p ->
    match p.result with
    | Finished f -> Some f.value
    | Stepped  _ -> None
  ) particles in
  let n = List.length finished in
  let mean_m = List.fold_left (fun acc (m, _) -> acc +. m) 0. finished /. float_of_int n in
  let mean_c = List.fold_left (fun acc (_, c) -> acc +. c) 0. finished /. float_of_int n in
  (mean_m, mean_c)

(* extract (trans_p, obs_p) posterior mean from a particle list *)
let pf_hmm_posterior_mean (particles : (float * float) particle list) =
  let finished = List.filter_map (fun p ->
    match p.result with
    | Finished f -> Some f.value
    | Stepped  _ -> None
  ) particles in
  let n = List.length finished in
  let mean_t = List.fold_left (fun acc (t, _) -> acc +. t) 0. finished /. float_of_int n in
  let mean_o = List.fold_left (fun acc (_, o) -> acc +. o) 0. finished /. float_of_int n in
  (mean_t, mean_o)

(* extract posterior mean from pmh chain *)
let pmh_posterior_mean chain burn_in =
  let samples = List.filteri (fun i _ -> i > burn_in) chain in
  let n_s = List.length samples in
  let mean_m = List.fold_left (fun acc ((m,_),_,_) -> acc +. m) 0. samples /. float_of_int n_s in
  let mean_c = List.fold_left (fun acc ((_,c),_,_) -> acc +. c) 0. samples /. float_of_int n_s in
  (mean_m, mean_c)

let pmh_hmm_posterior_mean chain burn_in =
  let samples = List.filteri (fun i _ -> i > burn_in) chain in
  let n_s = List.length samples in
  let mean_t = List.fold_left (fun acc ((t,_),_,_) -> acc +. t) 0. samples /. float_of_int n_s in
  let mean_o = List.fold_left (fun acc ((_,o),_,_) -> acc +. o) 0. samples /. float_of_int n_s in
  (mean_t, mean_o)

(* ---- tests ---- *)

let () =
  Random.init 42;

  let n_particles = 100  in
  let n_mhsteps   = 5    in
  let burn_in     = 100  in
  let n_pmh       = 1000 in

  (* --- LinReg 20 points --- *)
  Printf.printf "=== LIN REGR (20 points, true m=2.0 c=1.0) ===\n";
  let xs = List.init 20 (fun i -> float_of_int i /. 10.0 -. 2.0) in
  let ys = List.map (fun x -> 2.0 *. x +. 1.0 +. (Random.float 2.0 -. 1.0)) xs in

  let (mean_m, mean_c) = pf_posterior_mean (mpf n_particles (lin_regr_full xs ys)) in
  Printf.printf "MPF:  E[m]=%.3f (true=2.0) E[c]=%.3f (true=1.0)\n" mean_m mean_c;

  let (mean_m, mean_c) = pf_posterior_mean (rmpf n_particles n_mhsteps (lin_regr_full xs ys)) in
  Printf.printf "RMPF: E[m]=%.3f (true=2.0) E[c]=%.3f (true=1.0)\n" mean_m mean_c;

  let (mean_m, mean_c) = pmh_posterior_mean (pmh n_pmh n_particles (lin_regr_full xs ys)) burn_in in
  Printf.printf "PMH:  E[m]=%.3f (true=2.0) E[c]=%.3f (true=1.0)\n\n" mean_m mean_c;

  (* --- HMM 20 steps --- *)
  Printf.printf "=== HMM (20 steps, true trans_p=0.2 obs_p=0.9) ===\n";
  let ys_hmm = simulate_hmm 20 0 0.2 0.9 in

  let (mean_t, mean_o) = pf_hmm_posterior_mean (mpf n_particles (hmm 20 0 ys_hmm)) in
  Printf.printf "MPF:  E[trans_p]=%.3f (true=0.2) E[obs_p]=%.3f (true=0.9)\n" mean_t mean_o;

  let (mean_t, mean_o) = pf_hmm_posterior_mean (rmpf n_particles n_mhsteps (hmm 20 0 ys_hmm)) in
  Printf.printf "RMPF: E[trans_p]=%.3f (true=0.2) E[obs_p]=%.3f (true=0.9)\n" mean_t mean_o;

  let (mean_t, mean_o) = pmh_hmm_posterior_mean (pmh n_pmh n_particles (hmm 20 0 ys_hmm)) burn_in in
  Printf.printf "PMH:  E[trans_p]=%.3f (true=0.2) E[obs_p]=%.3f (true=0.9)\n" mean_t mean_o