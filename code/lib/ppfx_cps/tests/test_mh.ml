open Effects
open Mh
open Lin_regr

let mean xs =
  List.fold_left ( +. ) 0.0 xs /. float_of_int (List.length xs)

(* --------------------------------------------------------- *)
(* Test 1: Basic IM output                                   *)
(* --------------------------------------------------------- *)
let test_basic () =
  Printf.printf "=== Test 1: Basic IM ===\n";
  Random.init 42;
  let results = im 100 (lin_regr 0.5 0.1) in
  Printf.printf "Samples: %d\n" (List.length results);
  let ms = List.map (fun ((m, _), _) -> m) results in
  let cs = List.map (fun ((_, c), _) -> c) results in
  Printf.printf "Mean m: %.4f\n" (mean ms);
  Printf.printf "Mean c: %.4f\n\n" (mean cs)

(* --------------------------------------------------------- *)
(* Test 2: Convergence — error should decrease with steps    *)
(* True values: m=0.2, c=0.0 for x=0.5, y=0.1              *)
(* --------------------------------------------------------- *)
let test_convergence () =
  Printf.printf "=== Test 2: Convergence with steps ===\n";
  Printf.printf "%-10s %-12s %-12s\n" "Steps" "m error" "c error";
  Printf.printf "%s\n" (String.make 36 '-');
  let step_counts = [10; 50; 100; 500; 1000; 5000] in
  List.iter (fun n ->
    Random.init 42;
    let results = im n (lin_regr 0.5 0.1) in
    let ms = List.map (fun ((m, _), _) -> m) results in
    let cs = List.map (fun ((_, c), _) -> c) results in
    (* true posterior mean roughly m~0.2, c~0.0 for this data *)
    let err_m = abs_float (mean ms -. 0.2) in
    let err_c = abs_float (mean cs -. 0.0) in
    Printf.printf "%-10d %-12.4f %-12.4f\n" n err_m err_c
  ) step_counts;
  Printf.printf "\n"

(* --------------------------------------------------------- *)
(* Test 3: Timing — varying step count                       *)
(* --------------------------------------------------------- *)
let time_ms f =
  let counter = Mtime_clock.counter () in
  let r       = f () in
  let elapsed = Mtime_clock.count counter in
  let ms      = Mtime.Span.to_float_ns elapsed /. 1_000_000.0 in
  (ms, r)

let test_timing_steps () =
  Printf.printf "=== Test 3: Timing vs step count ===\n";
  Printf.printf "%-10s %-15s\n" "Steps" "Time (ms)";
  Printf.printf "%s\n" (String.make 26 '-');
  let step_counts = [100; 500; 1000; 2000; 5000] in
  List.iter (fun n ->
    Random.init 42;
    let (ms, _) = time_ms (fun () -> im n (lin_regr 0.5 0.1)) in
    Printf.printf "%-10d %-15.2f\n" n ms
  ) step_counts;
  Printf.printf "\n"

(* --------------------------------------------------------- *)
(* Test 4: Timing — varying model size (data points)         *)
(* --------------------------------------------------------- *)
let lin_regr_multi (xs : float list) (ys : float list)
    : (float sample_cap * float observe_cap, float * float) model =
  let addr_m   = Addr.make () in
  let addr_c   = Addr.make () in
  let addrs_obs = List.map (fun _ -> Addr.make ()) xs in
  Model (fun ((module S), (module O)) ->
    let m = Effect.perform (S.Sample (Dist.normal 0. 3., addr_m)) in
    let c = Effect.perform (S.Sample (Dist.normal 0. 2., addr_c)) in
    List.iter2 (fun x y ->
      let addr = List.assoc x (List.combine xs addrs_obs) in
      ignore (Effect.perform (O.Observe (Dist.normal (m *. x +. c) 1., addr, y))))
      xs ys;
    (m, c))

let test_timing_model_size () =
  Printf.printf "=== Test 4: Timing vs model size ===\n";
  Printf.printf "Fixed steps: 500\n";
  Printf.printf "%-15s %-15s\n" "Data points" "Time (ms)";
  Printf.printf "%s\n" (String.make 32 '-');
  let sizes = [1; 5; 10; 20; 50; 100] in
  List.iter (fun size ->
    Random.init 42;
    let xs = List.init size (fun i -> float_of_int i) in
    let ys = List.map (fun x -> 0.2 *. x) xs in
    let model = lin_regr_multi xs ys in
    let (ms, _) = time_ms (fun () -> im 500 model) in
    Printf.printf "%-15d %-15.2f\n" size ms
  ) sizes;
  Printf.printf "\n"

(* --------------------------------------------------------- *)
(* Main                                                      *)
(* --------------------------------------------------------- *)
let () =
  test_basic ();
  test_convergence ();
  test_timing_steps ();
  test_timing_model_size ()