(* Debug MPF - Trace exactly what's happening *)

open Effects
open Types
open Debug_multpf
open Printf

let fresh_addr (counter : int ref) (tag : string) : address =
  let local = !counter in
  incr counter;
  make_addr tag local

(* Simpler model: just 2 observations to trace carefully *)
let simple_linregr : (float * float) model =
  fun () ->
    printf "[MODEL] Starting model execution\n%!";
    let counter = ref 0 in
    
    printf "[MODEL] Sampling m\n%!";
    let m = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "m"
              ; dist = Dist.Normal (0.0, 3.0) }) in
    printf "[MODEL] Got m = %.4f\n%!" m;
    
    printf "[MODEL] Sampling c\n%!";
    let c = Effect.perform (FloatEffects.Sample
              { addr = fresh_addr counter "c"
              ; dist = Dist.Normal (0.0, 5.0) }) in
    printf "[MODEL] Got c = %.4f\n%!" c;
    
    printf "[MODEL] Observing at x=1.0, y=3.0\n%!";
    Effect.perform (FloatEffects.Observe
              { addr = fresh_addr counter "obs1"
              ; dist = Dist.Normal (m *. 1.0 +. c, 1.0)
              ; obs  = 3.0 });
    printf "[MODEL] After first observe\n%!";
    
    printf "[MODEL] Observing at x=2.0, y=6.0\n%!";
    Effect.perform (FloatEffects.Observe
              { addr = fresh_addr counter "obs2"
              ; dist = Dist.Normal (m *. 2.0 +. c, 1.0)
              ; obs  = 6.0 });
    printf "[MODEL] After second observe\n%!";
    
    printf "[MODEL] Returning (m=%.4f, c=%.4f)\n%!" m c;
    (m, c)

let () =
  Random.init 42;
  
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  MPF Debug Trace - 3 Particles, 2 Observations        ║\n";
  printf "╚════════════════════════════════════════════════════════╝\n\n";
  
  printf "Running mulpfilter with 3 particles...\n\n";
  let results = mulpfilter 3 simple_linregr in
  
  printf "\n\n=== RESULTS ===\n";
  List.iteri (fun i ((m, c), lw) ->
    printf "Particle %d: m=%.4f, c=%.4f, log_weight=%.4f\n" (i+1) m c lw
  ) results
