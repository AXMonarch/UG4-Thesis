(** Main entry point demonstrating probabilistic models and inference *)

let () =
  Random.init 42;
  print_endline "=== Probabilistic Programming with Effect Handlers ===";
  print_endline "";
  
  (* Particle Filter demos *)
  print_endline "--- Particle Filter Variants ---";
  Pf.Pf_base.demo_toy_hmm ();
  Pf.Multinomial_pf.demo_multinomial_pf ();
  Pf.Resample_move_pf.demo_resample_move_pf ();
  
  (* Metropolis-Hastings demos *)
  print_endline "\n--- Metropolis-Hastings Variants ---";
  Mh.Mh_base.demo_toy_hmm ();
  Mh.Independence_mh.demo_independence_mh ();
  Mh.Single_site_mh.demo_single_site_mh ();
  Mh.Particle_mh.demo_particle_mh ();
  
  print_endline "\n=== All Demos Complete ==="
