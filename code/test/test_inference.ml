[@@@ocaml.warning "-27-33-39"]

open Alcotest
open Effect
open Effects
open Effect.Deep

let () = Random.init 42  


let make_test_model obs_val =
  fun () ->
    let state = perform (Sample { name = "state"; dist = Uniform (0.0, 2.0) }) in
    perform (Observe { name = "obs"; dist = Normal (state, 0.5); obs = obs_val });
    state

let make_coin_model obs_val =
  fun () ->
    let bias = perform (Sample { name = "bias"; dist = Uniform (0.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (bias, 0.1); obs = obs_val });
    bias

let make_2d_model obs_x obs_y =
  fun () ->
    let x = perform (Sample { name = "x"; dist = Normal (0.0, 1.0) }) in
    let y = perform (Sample { name = "y"; dist = Normal (0.0, 1.0) }) in
    perform (Observe { name = "obs_x"; dist = Normal (x, 0.5); obs = obs_x });
    perform (Observe { name = "obs_y"; dist = Normal (y, 0.5); obs = obs_y });
    (x, y)


let test_mh_forward_sample () =
  let model = make_test_model 1.5 in
  let (trace, result) = Mh.Mh_base.forward_sample model in
  check bool "Forward sample returns result" true (result >= 0.0 && result <= 2.0);
  check bool "Trace has choices" true (Hashtbl.length trace.choices > 0)

let test_mh_run () =
  let model = make_test_model 1.5 in
  let propose_fn _name _dist current = current +. (Random.float 0.4 -. 0.2) in
  let traces = Mh.Mh_base.run_mh ~iters:50 ~propose:propose_fn model in
  check int "Correct number of traces" 50 (List.length traces);  check bool "Has results" true (List.length traces > 0)

let test_independence_mh () =
  let model = make_coin_model 0.7 in
  let traces = Mh.Independence_mh.run_independence_mh model 100 in
  check int "Correct number of traces" 100 (List.length traces);
  (* Check that we can extract samples *)
  let samples = List.filteri (fun i _ -> i >= 50) traces in
  check bool "Has burn-in samples" true (List.length samples > 0)

let test_single_site_mh () =
  let model = make_2d_model 1.0 2.0 in
  let results = Mh.Single_site_mh.run_single_site_mh model 100 0.3 in
  check int "Correct number of results" 100 (List.length results);
  check bool "Has results" true (List.length results > 0)


let test_particle_mh () =
  let model = make_test_model 1.5 in
  let results = Mh.Particle_mh.run_particle_mh model 20 5 0.3 in
  check int "Correct number of results" 20 (List.length results);
  check bool "Has results" true (List.length results > 0)


let test_pf_normalize_weights () =
  let cloud = Array.init 5 (fun _ -> Pf.Pf_base.empty_particle ()) in
  cloud.(0) <- { cloud.(0) with log_weight = log 2.0 };
  cloud.(1) <- { cloud.(1) with log_weight = log 3.0 };
  let normalized = Pf.Pf_base.normalize_log_weights cloud in
  let total = Array.fold_left (fun acc (_, w) -> acc +. w) 0.0 normalized in
  check (float 0.001) "Weights sum to 1" 1.0 total

let test_pf_ess () =
  let cloud = Array.init 10 (fun _ -> Pf.Pf_base.empty_particle ()) in
  let normalized = Pf.Pf_base.normalize_log_weights cloud in
  let ess = Pf.Pf_base.effective_sample_size normalized in
  check (float 0.1) "ESS equals N for uniform weights" 10.0 ess


let test_multinomial_pf_run () =
  let model () =
    let pos = perform (Sample { name = "pos"; dist = Normal (5.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (pos, 0.5); obs = 5.5 });
    pos
  in
  let cloud = Pf.Multinomial_pf.run_multinomial_pf model 10 0.5 in
  check int "Correct number of particles" 10 (Array.length cloud)

let test_resample_move_pf () =
  let model () =
    let state = perform (Sample { name = "state"; dist = Normal (3.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (state, 0.5); obs = 4.0 });
    state
  in
  let cloud = Pf.Resample_move_pf.run_resample_move_pf model 10 2 0.5 0.5 in
  check int "Correct number of particles" 10 (Array.length cloud)

let suite = [
  ("mh_base", [
    test_case "forward_sample" `Quick test_mh_forward_sample;
    test_case "run_mh" `Quick test_mh_run;
  ]);
  ("independence_mh", [
    test_case "run_independence_mh" `Quick test_independence_mh;
  ]);
  ("single_site_mh", [
    test_case "run_single_site_mh" `Quick test_single_site_mh;
  ]);
  ("particle_mh", [
    test_case "run_particle_mh" `Quick test_particle_mh;
  ]);
  ("pf_base", [
    test_case "normalize_weights" `Quick test_pf_normalize_weights;
    test_case "effective_sample_size" `Quick test_pf_ess;
  ]);
  ("multinomial_pf", [
    test_case "run_multinomial_pf" `Quick test_multinomial_pf_run;
  ]);
  ("resample_move_pf", [
    test_case "run_resample_move_pf" `Quick test_resample_move_pf;
  ]);
]
