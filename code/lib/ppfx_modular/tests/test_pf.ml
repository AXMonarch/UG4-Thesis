(* Text-based visualiser for mulpfilter on linear regression  *)
(* Shows:                                                     *)
(*   - particle estimates of (m, c) after inference           *)
(*   - weight distribution across particles                   *)
(*   - effective sample size                                  *)


open Lin_regr
open Multpf

(* ---------------------------------------------------------- *)
(* Effective sample size                                      *)
(* ESS = (sum w_i)^2 / sum(w_i^2)                            *)
(* In log space: exp(2*log_sum - log_sum_sq)                  *)
(* Higher ESS = more diverse particles                        *)
(* ---------------------------------------------------------- *)
let effective_sample_size (log_ws : float list) : float =
  let max_lw  = List.fold_left max neg_infinity log_ws in
  let ws      = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
  let sum_w   = List.fold_left ( +. ) 0.0 ws in
  let sum_w2  = List.fold_left (fun acc w -> acc +. w *. w) 0.0 ws in
  (sum_w *. sum_w) /. sum_w2

(* ---------------------------------------------------------- *)
(* Text visualiser helpers                                    *)
(* ---------------------------------------------------------- *)

(* bar : float -> int -> string                               *)
(* draws a simple ascii bar proportional to value            *)
let bar (value : float) (max_width : int) : string =
  let n = int_of_float (value *. float_of_int max_width) in
  let n = max 0 (min n max_width) in
  String.make n '#'

(* print_particles : (float * float) * float) list -> unit    *)
let print_particles (results : ((float * float) * float) list) : unit =
  let n       = List.length results in
  let log_ws  = List.map snd results in
  let max_lw  = List.fold_left max neg_infinity log_ws in
  let ws_norm = List.map (fun lw -> exp (lw -. max_lw)) log_ws in
  let sum_w   = List.fold_left ( +. ) 0.0 ws_norm in
  let ws_norm = List.map (fun w -> w /. sum_w) ws_norm in

  Printf.printf "\n=== Particle Filter Results ===\n";
  Printf.printf "Particles: %d\n\n" n;

  (* per-particle estimates *)
  Printf.printf "%-6s %-10s %-10s %-10s %s\n"
    "Idx" "m (slope)" "c (intcpt)" "weight" "rel. weight";
  Printf.printf "%s\n" (String.make 60 '-');
  List.iteri (fun i ((m, c), _lw) ->
    let w_rel = List.nth ws_norm i in
    Printf.printf "%-6d %-10.4f %-10.4f %-10.6f |%s\n"
      i m c w_rel (bar w_rel 20)
  ) results;

  (* summary statistics *)
  let ms    = List.map (fun ((m, _), _) -> m) results in
  let cs    = List.map (fun ((_, c), _) -> c) results in
  let mean xs =
    List.fold_left ( +. ) 0.0 xs /. float_of_int (List.length xs)
  in
  let weighted_mean xs ws =
    List.fold_left2 (fun acc x w -> acc +. x *. w) 0.0 xs ws
  in
  let ess = effective_sample_size log_ws in

  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Mean m (unweighted): %.4f\n" (mean ms);
  Printf.printf "Mean c (unweighted): %.4f\n" (mean cs);
  Printf.printf "Mean m (weighted):   %.4f\n" (weighted_mean ms ws_norm);
  Printf.printf "Mean c (weighted):   %.4f\n" (weighted_mean cs ws_norm);
  Printf.printf "ESS: %.2f / %d (%.1f%%)\n"
    ess n (100.0 *. ess /. float_of_int n);
  Printf.printf "%s\n" (String.make 60 '=')

(* ---------------------------------------------------------- *)
(* Run the test                                               *)
(* ---------------------------------------------------------- *)
let () =
  Random.self_init ();

  (* single data point: x=1.0, true m=2.0, true c=1.0, y=3.0 *)
  let x = 1.0 in
  let y = 3.0 in
  let n_particles = 20 in

  Printf.printf "Model: y = m*x + c\n";
  Printf.printf "Data:  x=%.1f, y=%.1f\n" x y;
  Printf.printf "Prior: m ~ Normal(0,3), c ~ Normal(0,2)\n";
  Printf.printf "Running mulpfilter with %d particles...\n" n_particles;

  (* mulpfilter returns (float * float) * float) list          *)
  (* where float * float = (m, c) and float = log weight       *)
  let model   = lin_regr x y in
  let results = mulpfilter n_particles model in

  print_particles results