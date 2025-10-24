open Code.Linreg

let () =
  let open Printf in
  Random.self_init ();
  let dataset = [ (1.0, 3.0); (2.0, 5.0); (3.0, 7.0); (4.0, 9.0); (5.0, 11.0) ] in
  printf "Running Metropolis–Hastings for linear regression...\n";
  let _ = mh_handle (fun () -> lin_reg_model dataset) in
  let samples = mh_iter dataset 100 (0.0, 0.0) [] in
  let ms = List.map fst samples and cs = List.map snd samples in
  printf "\n--- Results ---\n";
  printf "Posterior mean m ≈ %.3f\n" (mean ms);
  printf "Posterior mean c ≈ %.3f\n" (mean cs);
  printf "(True values: m=2.0, c=1.0)\n"