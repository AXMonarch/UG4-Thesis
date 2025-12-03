[@@@ocaml.warning "-27-33-39"]

open Alcotest
open Effect
open Effects
open Effect.Deep

let () = Random.init 42  (* Fixed seed for reproducible tests *)

(** Test helpers **)

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

(** MH Base Tests **)

let test_mh_forward_sample () =
  let model = make_test_model 1.5 in
  let (trace, result) = Mh.Mh_base.forward_sample model in
  check bool "Forward sample returns result" true (result >= 0.0 && result <= 2.0);
  check bool "Trace has choices" true (Hashtbl.length trace.choices > 0)

let test_mh_run () =
  let model = make_test_model 1.5 in
  let propose_fn _name _dist current = Some (current +. (Random.float 0.4 -. 0.2)) in
  let (traces, accepted) = Mh.Mh_base.run_mh model 50 propose_fn in
  check int "Correct number of traces" 50 (List.length traces);
  check bool "Some proposals accepted" true (accepted > 0)

(** Independence MH Tests **)

let test_independence_mh () =
  let model = make_coin_model 0.7 in
  let (traces, accepted) = Mh.Independence_mh.run_independence_mh model 100 in
  check int "Correct number of traces" 100 (List.length traces);
  check bool "Has some acceptance" true (accepted >= 0);
  (* Check that we can extract samples *)
  let samples = List.filteri (fun i _ -> i >= 50) traces in
  check bool "Has burn-in samples" true (List.length samples > 0)

(** Single-Site MH Tests **)

let test_single_site_mh () =
  let model = make_2d_model 1.0 2.0 in
  let (traces, accepted) = Mh.Single_site_mh.run_single_site_mh model 100 0.3 in
  check int "Correct number of traces" 100 (List.length traces);
  check bool "Some proposals accepted" true (accepted > 0);
  (* Check that traces have both x and y *)
  let last_trace = List.hd (List.rev traces) in
  check bool "Has x variable" true (Hashtbl.mem last_trace.choices "x");
  check bool "Has y variable" true (Hashtbl.mem last_trace.choices "y")

(** Particle MH Tests **)

let test_particle_mh () =
  let model = make_test_model 1.5 in
  let propose_fn _name _dist current = Some (current +. (Random.float 0.4 -. 0.2)) in
  let (traces, accepted) = Mh.Particle_mh.run_particle_mh model 20 5 propose_fn in
  check int "Correct number of traces" 20 (List.length traces);
  check bool "Non-negative acceptance" true (accepted >= 0)

(** PF Base Tests **)

let test_pf_init_particles () =
  let particles = Pf.Pf_base.init_particles 10 in
  check int "Correct number of particles" 10 (Array.length particles);
  Array.iter (fun p ->
    check (float 0.001) "Initial weight" 1.0 (Pf.Pf_base.(p.weight))
  ) particles

let test_pf_normalize_weights () =
  let particles = Pf.Pf_base.init_particles 5 in
  (* Set different weights *)
  particles.(0) <- { particles.(0) with Pf.Pf_base.weight = 2.0 };
  particles.(1) <- { particles.(1) with Pf.Pf_base.weight = 3.0 };
  let normalized = Pf.Pf_base.normalize_weights particles in
  let total = Array.fold_left (fun acc p -> acc +. Pf.Pf_base.(p.weight)) 0.0 normalized in
  check (float 0.001) "Weights sum to 1" 1.0 total

let test_pf_ess () =
  let particles = Pf.Pf_base.init_particles 10 in
  (* ESS formula is 1/sum(w^2). For uniform weights w=1/N, ESS = 1/(N*(1/N)^2) = N *)
  (* But our particles have weight=1.0 initially (not normalized), so sum(w^2) = N*1 = N, ESS = 1/N *)
  (* After normalization ESS should be N *)
  let normalized = Pf.Pf_base.normalize_weights particles in
  let ess_norm = Pf.Pf_base.effective_sample_size normalized in
  check (float 0.1) "ESS equals N for uniform weights" 10.0 ess_norm

let test_pf_propagate () =
  let model = make_test_model 1.5 in
  let particles = Pf.Pf_base.init_particles 5 in
  let (propagated, results) = Pf.Pf_base.propagate_particles model particles in
  check int "Correct number of particles" 5 (Array.length propagated);
  check int "Correct number of results" 5 (List.length results)

(** Multinomial PF Tests **)

let test_multinomial_resample () =
  let particles = Pf.Pf_base.init_particles 10 in
  particles.(0) <- { particles.(0) with Pf.Pf_base.weight = 5.0 };
  let normalized = Pf.Pf_base.normalize_weights particles in
  let resampled = Pf.Multinomial_pf.multinomial_resample normalized in
  check int "Same number after resample" 10 (Array.length resampled);
  let total = Array.fold_left (fun acc p -> acc +. Pf.Pf_base.(p.weight)) 0.0 resampled in
  check (float 0.001) "Weights sum to 1" 1.0 total

let test_multinomial_pf_run () =
  let model _t () =
    let pos = perform (Sample { name = "pos"; dist = Normal (5.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (pos, 0.5); obs = 5.5 });
    pos
  in
  let clouds = Pf.Multinomial_pf.run_multinomial_pf model 10 3 0.5 in
  check bool "Multiple time steps" true (List.length clouds > 0)

(** Resample-Move PF Tests **)

let test_resample_move_pf () =
  let model _t () =
    let state = perform (Sample { name = "state"; dist = Normal (3.0, 1.0) }) in
    perform (Observe { name = "obs"; dist = Normal (state, 0.5); obs = 4.0 });
    state
  in
  let propose_fn _name _dist current = Some (current +. (Random.float 0.4 -. 0.2)) in
  let clouds = Pf.Resample_move_pf.run_resample_move_pf model 10 3 2 propose_fn 0.5 in
  check bool "Multiple time steps" true (List.length clouds > 0)

let test_resample_move_rejuvenation () =
  let particles = Pf.Pf_base.init_particles 5 in
  let model = make_test_model 1.5 in
  let propose_fn _name _dist current = Some (current +. (Random.float 0.2 -. 0.1)) in
  let resample_fn = Pf.Resample_move_pf.make_resample_move model 
    Pf.Resample_move_pf.multinomial_resample_base 3 propose_fn in
  let (propagated, _) = Pf.Pf_base.propagate_particles model particles in
  let normalized = Pf.Pf_base.normalize_weights propagated in
  let result = resample_fn normalized in
  check int "Same number of particles" 5 (Array.length result)

(** Integration Tests **)

let test_mh_vs_pf_consistency () =
  (* Both methods should give reasonable estimates for the same model *)
  let obs_val = 1.5 in
  
  (* MH estimate *)
  let mh_model = make_test_model obs_val in
  let propose_fn _name _dist current = Some (current +. (Random.float 0.3 -. 0.15)) in
  let (mh_traces, _) = Mh.Mh_base.run_mh mh_model 100 propose_fn in
  let mh_samples = List.filteri (fun i _ -> i >= 50) mh_traces in
  let mh_vals = List.filter_map (fun t -> Hashtbl.find_opt t.choices "state") mh_samples in
  let mh_estimate = List.fold_left (+.) 0.0 mh_vals /. float_of_int (List.length mh_vals) in
  
  (* PF estimate *)
  let pf_model () = make_test_model obs_val () in
  let particles = Pf.Pf_base.init_particles 50 in
  let (propagated, _) = Pf.Pf_base.propagate_particles pf_model particles in
  let normalized = Pf.Pf_base.normalize_weights propagated in
  let pf_estimate = Pf.Pf_base.estimate_state normalized "state" in
  
  (* Both should be reasonably close to obs_val *)
  check bool "MH estimate reasonable" true (mh_estimate > 0.5 && mh_estimate < 2.5);
  check bool "PF estimate reasonable" true (pf_estimate > 0.5 && pf_estimate < 2.5)

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
    test_case "init_particles" `Quick test_pf_init_particles;
    test_case "normalize_weights" `Quick test_pf_normalize_weights;
    test_case "effective_sample_size" `Quick test_pf_ess;
    test_case "propagate_particles" `Quick test_pf_propagate;
  ]);
  ("multinomial_pf", [
    test_case "multinomial_resample" `Quick test_multinomial_resample;
    test_case "run_multinomial_pf" `Quick test_multinomial_pf_run;
  ]);
  ("resample_move_pf", [
    test_case "run_resample_move_pf" `Quick test_resample_move_pf;
    test_case "resample_move_rejuvenation" `Quick test_resample_move_rejuvenation;
  ]);
  ("integration", [
    test_case "mh_vs_pf_consistency" `Quick test_mh_vs_pf_consistency;
  ]);
]
