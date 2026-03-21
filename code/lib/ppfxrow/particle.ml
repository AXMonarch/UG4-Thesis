[@@@ocaml.warning "-26-27-32-39"]

(* ============================================================
   A simple test of continuation-based particle suspension
   without advance_result
   ============================================================ *)

type _ Effect.t += Observe : float * float -> float Effect.t
(* Observe (dist_value, observed) -> log_prob *)

(* Particle type — no advance_result needed *)
type 'a particle =
  | Done      of { value : 'a; weight : float }
  | Suspended of { resume : unit -> 'a particle; weight : float }

(* Step a model forward by one Observe *)
let rec advance_one (f : unit -> 'a) : 'a particle =
  match f () with
  | result ->
      Done { value = result; weight = 0. }
  | effect (Observe (x, y)), k ->
      let lp = log (if x > 0. then x else 1e-10) +. y in
      Suspended
        { weight = lp
        ; resume = fun () -> Effect.Deep.continue k x }
        
(* A simple model with two observations *)
let simple_model () =
  let x1 = Effect.perform (Observe (0.7, 1.0)) in
  let x2 = Effect.perform (Observe (0.3, 0.5)) in
  (x1, x2)

(* Run through all steps manually *)
let () =
  Printf.printf "=== Continuation Suspension Test ===\n";
  let p0 = advance_one simple_model in
  match p0 with
  | Done { value = (x1, x2); weight } ->
      Printf.printf "Done immediately: x1=%.4f x2=%.4f w=%.4f\n" x1 x2 weight
  | Suspended { weight = w1; resume } ->
      Printf.printf "Suspended at Observe 1: weight=%.4f\n" w1;
      let p1 = resume () in
      match p1 with
      | Done { value = (x1, x2); weight } ->
          Printf.printf "Done immediately after resume: x1=%.4f x2=%.4f w=%.4f\n" x1 x2 weight
      | Suspended { weight = w2; resume = resume2 } ->
          Printf.printf "Suspended at Observe 2: weight=%.4f\n" w2;
          let p2 = resume2 () in
          match p2 with
          | Done { value = (x1, x2); weight } ->
              Printf.printf "Done: x1=%.4f x2=%.4f final_weight=%.4f\n" x1 x2 weight
          | Suspended _ ->
              Printf.printf "Still suspended — unexpected\n"