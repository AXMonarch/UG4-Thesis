(* Finished means the model ran to completion *)
(* Stepped means the model hits an Observe site 
We have the log weight accumulated so far, 
and next_particle which is 
the rest of the computation from that point.
next_particle` is itself an 
a advance_result not a unit -> a'(model)
The suspension is already embedded in the value.*)

(*Now, we carry the trace to be passed into 
the composition of handlers and stopped and resumed.*)
[@@@ocaml.warning "-27-32-39"]

open Mh

type 'a advance_result =
  | Stepped  of { next_particle : 'a advance_result
                ; weight        : float
                ; trace         : Trace.t }
  | Finished of { value         : 'a
                ; weight        : float
                ; trace         : Trace.t }


(* First we make a reference to a trace (tr0) to store
the trace as we go along. We get its value with !tr

Then we match it with the possibilities :
1) x -> Finished means model ran to completion
x could also sample from a dist, or observe,
which we have the effects for.

!tr will now have the full list of addresses 
accessed in the run. 


2) In the case of sample, we 
try to insert a random value into the trace, 
and then make a continuation with the drawn value.

!tr gets updated with the new address and value, 
and we continue the computation with the drawn value.


3) For observe, we compute log-prob and continue 
the rest of computation, returning a stepped value
which has the next_particle as the rest of the computation.

!tr has every Sample uptil then. We capture it here
and suspend the rest of the computation as next_particle,
with the log weight and trace.
Continue k () runs to the next observe, 
building a new particle with the updated trace and weight

*)

let advance : 'b. Trace.t
              -> (< sample : 'a. 'a Dist.t -> 'a;
                   observe : 'a. 'a Dist.t -> 'a -> unit; .. > -> 'b)
              -> 'b advance_result
  = fun tr0 model ->
    let tr = ref tr0 in
    match model base_cap with
    | x -> Finished { value = x; weight = 0.; trace = !tr }
    | effect (Sample (d, addr)), k ->
        let r = Trace.try_insert addr (Random.float 1.0) tr in
        Effect.Deep.continue k (Dist.draw r d)
    | effect (Observe (d, x, _)), k ->
        let lp   = Dist.log_prob x d in
        let rest = Effect.Deep.continue k () in
        Stepped { next_particle = rest
                ; weight        = lp
                ; trace         = !tr }

(* Each particle has a state whether 
its finished or stepped. 

Weight is the log weight accumulated so far,
and the trace is the trace up to that
*)                

type 'a particle = {
  result : 'a advance_result;
  weight : float;
}

(*Adding another effect*)
type _ Effect.t +=
  | Resample : 'a particle list -> 'a particle list Effect.t

(* What Resample actually is, it takes a list of 
particles and returns a new list of particles with 
resampled weights. It eliminates particles that 
have low weights and duplicates high-weight particles*)  
let resample : 'a particle list -> 'a particle list
  = fun particles ->
    let n = List.length particles in
    let log_weights = Array.of_list (List.map (fun p -> p.weight) particles) in
    let max_w = Array.fold_left max neg_infinity log_weights in
    let weights = Array.map (fun lw -> exp (lw -. max_w)) log_weights in
    let total = Array.fold_left ( +. ) 0. weights in
    let probs = Array.map (fun w -> w /. total) weights in
    let particles_arr = Array.of_list particles in
    List.init n (fun _ ->
      let r = Random.float 1.0 in
      let rec pick i cumsum =
        if i >= n - 1 then i
        else
          let cumsum' = cumsum +. probs.(i) in
          if r <= cumsum' then i
          else pick (i + 1) cumsum'
      in
      let idx = pick 0 0. in
      { particles_arr.(idx) with weight = 0. })

let pf : 'b. int
         -> Trace.t
         -> (< sample : 'a. 'a Dist.t -> 'a;
               observe : 'a. 'a Dist.t -> 'a -> unit; .. > -> 'b)
         -> 'b particle list
  = fun n_particles tr0 model ->
    let init = List.init n_particles (fun _ ->
      { result = advance tr0 model; weight = 0. })
    in
    let rec loop particles =
      match particles with
      | [] -> []
      | _ ->
        if List.for_all (fun p -> match p.result with
            | Finished _ -> true
            | Stepped  _ -> false) particles
        then particles
        else begin
          let weighted = List.map (fun p ->
            match p.result with
            | Stepped  s -> { p with weight = p.weight +. s.weight }
            | Finished _ -> p
          ) particles in
          let resampled = Effect.perform (Resample weighted) in
          let stepped = List.map (fun p ->
            match p.result with
            | Stepped  s -> { p with result = s.next_particle }
            | Finished _ -> p
          ) resampled in
          loop stepped
        end
    in
    loop init

let mpf : 'b. int
          -> (< sample : 'a. 'a Dist.t -> 'a;
                observe : 'a. 'a Dist.t -> 'a -> unit; .. > -> 'b)
          -> 'b particle list
  = fun n_particles model ->
    match pf n_particles Trace.empty model with
    | particles -> particles
    | effect (Resample ps), k ->
        let resampled = resample ps in
        Effect.Deep.continue k resampled

let rmpf : 'b. int
           -> int
           -> (< sample : 'a. 'a Dist.t -> 'a;
                 observe : 'a. 'a Dist.t -> 'a -> unit; .. > -> 'b)
           -> 'b particle list
  = fun n_particles n_mhsteps model ->
    match pf n_particles Trace.empty model with
    | particles -> particles
    | effect (Resample ps), k ->
        let resampled = resample ps in
        let moved = List.map (fun p ->
          match p.result with
          | Finished _ -> p
          | Stepped s ->
              let chain =
                match mh n_mhsteps s.trace exec_model model with
                | c -> c
                | effect (Propose tr), k ->
                    let addrs = Trace.addresses tr in
                    let n_addrs = List.length addrs in
                    if n_addrs = 0 then Effect.Deep.continue k tr
                    else
                      let addr = List.nth addrs (Random.int n_addrs) in
                      Effect.Deep.continue k (Trace.AddrMap.remove addr tr)
                | effect (Accept (w, w', n, n')), k ->
                    let ratio = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
                    Effect.Deep.continue k (ratio >= Random.float 1.0)
              in
              let (_, _, improved_trace) = List.hd chain in
              { p with result = Stepped { s with trace = improved_trace } }
        ) resampled in
        Effect.Deep.continue k moved

(* Hybrid Model : PMH *)

let exec_pf : 'b. int
              -> Trace.t
              -> (< sample : 'a. 'a Dist.t -> 'a;
                   observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b)
              -> 'b * float * Trace.t
  = fun n_particles _ model ->
    let particles = mpf n_particles model in
    let finished = List.filter_map (fun p ->
      match p.result with
      | Finished f -> Some (f.value, f.weight, f.trace)
      | Stepped  _ -> None
    ) particles in
    let weights = List.map (fun (_, w, _) -> w) finished in
    let max_w   = List.fold_left max neg_infinity weights in
    let log_z   = log (List.fold_left (fun acc w ->
      acc +. exp (w -. max_w)) 0. weights)
      +. max_w -. log (float_of_int n_particles) in
    let (value, _, trace) = List.nth finished
      (Random.int (List.length finished)) in
    (value, log_z, trace)

let pmh : 'b. int
          -> int
          -> (< sample : 'a. 'a Dist.t -> 'a;
                observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b)
          -> ('b * float * Trace.t) list
  = fun n_mhsteps n_particles model ->
    match mh n_mhsteps Trace.empty (exec_pf n_particles) model with
    | chain -> chain
    | effect (Propose tr), k ->
        let addrs   = Trace.addresses tr in
        let n_addrs = List.length addrs in
        if n_addrs = 0 then Effect.Deep.continue k tr
        else
          let i    = Random.int n_addrs in
          let addr = List.nth addrs i in
          let tr'  = Trace.AddrMap.remove addr tr in
          Effect.Deep.continue k tr'
    | effect (Accept (w, w', n, n')), k ->
        let ratio  = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
        let accept = ratio >= Random.float 1.0 in
        Effect.Deep.continue k accept