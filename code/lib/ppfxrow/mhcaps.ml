[@@@ocaml.warning "-27-32-39"]
open Mh
open Pf

(* ---- Type container for functor arguments ---- *)

module type R = sig
  type t
end

(* ---- Effect signatures ---- *)

module type SAMPLE = sig
  type a
  type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
end

module Sample = struct
  module Make(T : R) : SAMPLE with type a = T.t = struct
    type a = T.t
    type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
  end
end

module type OBSERVE = sig
  type a
  type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
end

module Observe = struct
  module Make(T : R) : OBSERVE with type a = T.t = struct
    type a = T.t
    type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
  end
end

(* ---- Model type ---- *)

type ('c, 'a) model = Model of ((< .. > as 'c) -> 'a) [@@unboxed]

let run_with_capabilities : (< .. > as 'c) -> ('c, 'a) model -> 'a
  = fun caps (Model m) -> m caps

(* ---- Polymorphic capability class types (supervisor style) ---- *)

class type ['a] sample_capability = object
  method sample : 'a Dist.t -> Addr.t -> 'a
end

class type ['a] observe_capability = object
  method observe : 'a Dist.t -> Addr.t -> 'a -> 'a
end

(* ---- Per-type capability class types ---- *)

class type sample_float_cap = object
  method sample_float : float Dist.t -> Addr.t -> float
end

class type sample_bool_cap = object
  method sample_bool : bool Dist.t -> Addr.t -> bool
end

class type sample_int_cap = object
  method sample_int : int Dist.t -> Addr.t -> int
end

class type sample_float_array_cap = object
  method sample_float_array : float array Dist.t -> Addr.t -> float array
end

class type observe_float_cap = object
  method observe_float : float Dist.t -> Addr.t -> float -> float
end

class type observe_int_cap = object
  method observe_int : int Dist.t -> Addr.t -> int -> int
end

(* ---- Model capability compositions ---- *)

class type linreg_cap = object
  inherit sample_float_cap
  inherit observe_float_cap
end

class type hmm_cap = object
  inherit sample_float_cap
  inherit sample_bool_cap
  inherit observe_int_cap
end

class type latdiri_cap = object
  inherit sample_float_array_cap
  inherit sample_int_cap
  inherit observe_int_cap
end

(* ---- Handler signatures ---- *)

module type SAMPLE_HANDLER = sig
  type a
  val run : (< sample : a Dist.t -> Addr.t -> a >, 'b) model -> 'b
end

module type OBSERVE_HANDLER = sig
  type a
  val run : (< observe : a Dist.t -> Addr.t -> a -> a >, 'b) model -> 'b
end

(* ---- Default handler functors ---- *)

module DefaultSample = struct
  module Make(T : R) : SAMPLE_HANDLER with type a = T.t = struct
    type a = T.t
    module S = Sample.Make(T)
    let capability : a sample_capability = object
      method sample d addr = Effect.perform (S.Sample (d, addr))
    end
    let run (Model m) =
      match m capability with
      | ans -> ans
      | effect S.Sample (dist, _), k ->
          let r = Random.float 1.0 in
          Effect.Deep.continue k (Dist.draw r dist)
  end
end

module DefaultObserve = struct
  module Make(T : R) : OBSERVE_HANDLER with type a = T.t = struct
    type a = T.t
    module O = Observe.Make(T)
    let capability : a observe_capability = object
      method observe d addr y = Effect.perform (O.Observe (d, addr, y))
    end
    let run (Model m) =
      match m capability with
      | ans -> ans
      | effect O.Observe (_, _, y), k ->
          Effect.Deep.continue k y
  end
end

(* ---- Effect module instantiations ---- *)

module SampleFloat      = Sample.Make(struct type t = float end)
module SampleBool       = Sample.Make(struct type t = bool end)
module SampleInt        = Sample.Make(struct type t = int end)
module SampleFloatArray = Sample.Make(struct type t = float array end)
module ObserveFloat     = Observe.Make(struct type t = float end)
module ObserveInt       = Observe.Make(struct type t = int end)

(* ---- Concrete capability classes ---- *)

class sample_float_class : sample_float_cap = object
  method sample_float d addr = Effect.perform (SampleFloat.Sample (d, addr))
end

class sample_bool_class : sample_bool_cap = object
  method sample_bool d addr = Effect.perform (SampleBool.Sample (d, addr))
end

class sample_int_class : sample_int_cap = object
  method sample_int d addr = Effect.perform (SampleInt.Sample (d, addr))
end

class sample_float_array_class : sample_float_array_cap = object
  method sample_float_array d addr = Effect.perform (SampleFloatArray.Sample (d, addr))
end

class observe_float_class : observe_float_cap = object
  method observe_float d addr x = Effect.perform (ObserveFloat.Observe (d, addr, x))
end

class observe_int_class : observe_int_cap = object
  method observe_int d addr x = Effect.perform (ObserveInt.Observe (d, addr, x))
end

(* ---- Composite base caps ---- *)

let linreg_base_cap : linreg_cap = object
  inherit sample_float_class
  inherit observe_float_class
end

let hmm_base_cap : hmm_cap = object
  inherit sample_float_class
  inherit sample_bool_class
  inherit observe_int_class
end

let latdiri_base_cap : latdiri_cap = object
  inherit sample_float_array_class
  inherit sample_int_class
  inherit observe_int_class
end

(* ---- Models ---- *)

let lin_regr_full (xs : float list) (ys : float list)
    (cap : #linreg_cap) : float * float =
  let m = cap#sample_float (Dist.normal 0. 3.) (Addr.make ()) in
  let c = cap#sample_float (Dist.normal 0. 5.) (Addr.make ()) in
  let s = cap#sample_float (Dist.uniform 1. 3.) (Addr.make ()) in
  List.iter2 (fun x y ->
    let _ = cap#observe_float (Dist.normal (m *. x +. c) s) (Addr.make ()) y in
    ()
  ) xs ys;
  (m, c)

let hmm_model (n : int) (x0 : int) (ys : int array)
    (cap : #hmm_cap)
    : float * float =
  let trans_p = cap#sample_float (Dist.beta 2. 2.) (Addr.make ()) in
  let obs_p   = cap#sample_float (Dist.beta 2. 2.) (Addr.make ()) in
  let x = ref x0 in
  for i = 0 to n - 1 do
    let dx = if cap#sample_bool (Dist.bernoulli trans_p) (Addr.make ()) then 1 else 0 in
    x := !x + dx;
    let _ = cap#observe_int (Dist.binomial !x obs_p) (Addr.make ()) ys.(i) in
    ()
  done;
  (trans_p, obs_p)

let vocab_size = 4
let n_topics   = 2

let simulate_lat_diri (n_words : int) : int array =
  Array.init n_words (fun i -> i mod vocab_size)

let lat_diri (n_words : int) (obs : int array) (cap : #latdiri_cap) : int array =
  let topic_word_ps = Array.init n_topics (fun _ ->
    cap#sample_float_array
      (Dist.dirichlet (Array.make vocab_size 1.0))
      (Addr.make ())
  ) in
  let doc_topic_ps =
    cap#sample_float_array
      (Dist.dirichlet (Array.make n_topics 1.0))
      (Addr.make ())
  in
  Array.init n_words (fun i ->
    let z = cap#sample_int
      (Dist.categorical doc_topic_ps)
      (Addr.make ())
    in
    let word_ps = topic_word_ps.(z) in
    cap#observe_int
      (Dist.categorical word_ps)
      (Addr.make ())
      obs.(i)
  )


module type REUSE_TRACE_HANDLER = sig
  type a
  val run : Trace.t ref -> (< sample : a Dist.t -> Addr.t -> a >, 'b) model -> 'b
end

module ReuseTrace = struct
  module Make(T : R) : REUSE_TRACE_HANDLER with type a = T.t = struct
    type a = T.t
    module S = Sample.Make(T)
    let capability : a sample_capability = object
      method sample d addr = Effect.perform (S.Sample (d, addr))
    end
    let run tr (Model m) =
      match m capability with
      | ans -> ans
      | effect S.Sample (d, addr), k ->
          let r = Trace.try_insert addr (Rng.next ()) tr in
          Effect.Deep.continue k (Dist.draw r d)
  end
end


module type LIKELIHOOD_HANDLER = sig
  type a
  val run : (< observe : a Dist.t -> Addr.t -> a -> a >, 'b) model -> 'b * float
end

module Likelihood = struct
  module Make(T : R) : LIKELIHOOD_HANDLER with type a = T.t = struct
    type a = T.t
    module O = Observe.Make(T)
    let capability : a observe_capability = object
      method observe d addr y = Effect.perform (O.Observe (d, addr, y))
    end
    let run (Model m) =
      let w = ref 0. in
      match m capability with
      | ans -> (ans, !w)
      | effect O.Observe (d, _, y), k ->
          w := !w +. Dist.log_prob y d;
          Effect.Deep.continue k y
  end
end

let exec_model_linreg (tr0 : Trace.t) (model : (linreg_cap, float * float) model)
    : (float * float) * float * Trace.t =
  let module RT = ReuseTrace.Make(struct type t = float end) in
  let module LH = Likelihood.Make(struct type t = float end) in
  let tr = ref tr0 in
  let (ans, w) =
    RT.run tr
      (Model (fun scap ->
        LH.run
          (Model (fun ocap ->
            let caps : linreg_cap = object
              method sample_float d addr = scap#sample d addr
              method observe_float d addr y = ocap#observe d addr y
            end in
            run_with_capabilities caps model))))
  in
  (ans, w, !tr)

let exec_model_hmm (tr0 : Trace.t) (model : (hmm_cap, float * float) model)
    : (float * float) * float * Trace.t =
  let module RTF = ReuseTrace.Make(struct type t = float end) in
  let module RTB = ReuseTrace.Make(struct type t = bool end) in
  let module LH  = Likelihood.Make(struct type t = int end) in
  let tr = ref tr0 in
  let (ans, w) =
    RTF.run tr
      (Model (fun scap_f ->
        RTB.run tr
          (Model (fun scap_b ->
            LH.run
              (Model (fun ocap ->
                let caps : hmm_cap = object
                  method sample_float d addr = scap_f#sample d addr
                  method sample_bool  d addr = scap_b#sample d addr
                  method observe_int  d addr y = ocap#observe d addr y
                end in
                run_with_capabilities caps model))))))
  in
  (ans, w, !tr)

let exec_model_latdiri (tr0 : Trace.t) (model : (latdiri_cap, int array) model)
    : int array * float * Trace.t =
  let module RTFA = ReuseTrace.Make(struct type t = float array end) in
  let module RTI  = ReuseTrace.Make(struct type t = int end) in
  let module LH   = Likelihood.Make(struct type t = int end) in
  let tr = ref tr0 in
  let (ans, w) =
    RTFA.run tr
      (Model (fun scap_fa ->
        RTI.run tr
          (Model (fun scap_i ->
            LH.run
              (Model (fun ocap ->
                let caps : latdiri_cap = object
                  method sample_float_array d addr = scap_fa#sample d addr
                  method sample_int         d addr = scap_i#sample d addr
                  method observe_int        d addr y = ocap#observe d addr y
                end in
                run_with_capabilities caps model))))))
  in
  (ans, w, !tr)
  (* ---- MH skeleton ---- *)

let mh : 'b. int
           -> Trace.t
           -> (Trace.t -> 'b * float * Trace.t)
           -> ('b * float * Trace.t) list
  = fun n tr0 exec ->
    let rec loop i state chain =
      if i >= n then chain
      else
        let (_, w, tr)     = state in
        let tr'            = Effect.perform (Propose tr) in
        let (x', w', tr'') = exec tr' in
        let accept         = Effect.perform (Accept (w, w', Trace.size tr, Trace.size tr'')) in
        let next           = if accept then (x', w', tr'') else state in
        loop (i + 1) next (next :: chain)
    in
    let init = exec tr0 in
    loop 0 init [init]

(* ---- IM handler ---- *)

let im : 'b. int
           -> (Trace.t -> 'b * float * Trace.t)
           -> ('b * float * Trace.t) list
  = fun n exec ->
    match mh n Trace.empty exec with
    | chain -> chain
    | effect (Propose tr), k ->
        let tr' = Trace.AddrMap.map (fun _ -> Rng.next ()) tr in
        Effect.Deep.continue k tr'
    | effect (Accept (w, w', _, _)), k ->
        let accept = exp (w' -. w) > Rng.next () in
        Effect.Deep.continue k accept

(* ---- SSMH handler ---- *)

let ssmh : 'b. int
             -> (Trace.t -> 'b * float * Trace.t)
             -> ('b * float * Trace.t) list
  = fun n exec ->
    match mh n Trace.empty exec with
    | chain -> chain
    | effect (Propose tr), k ->
      let addrs   = Trace.addresses tr in
      let n_addrs = List.length addrs in
      if n_addrs = 0 then Effect.Deep.continue k tr
      else
        let i    = int_of_float (Rng.next () *. float_of_int n_addrs) in
        let addr = List.nth addrs i in
        let new_r = Rng.next () in
        let tr'  = Trace.AddrMap.add addr new_r tr in
        Effect.Deep.continue k tr'
    | effect (Accept (w, w', n, n')), k ->
        let ratio  = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
        let accept = ratio > Rng.next () in
        Effect.Deep.continue k accept

(* ---- Particle filter handler ---- *)

module type ADVANCE_OBSERVE_HANDLER = sig
  type a
  val run : Trace.t ref 
            -> (< observe : a Dist.t -> Addr.t -> a -> a >, 'b advance_result) model 
            -> 'b advance_result
end

module AdvanceObserve = struct
  module Make(T : R) : ADVANCE_OBSERVE_HANDLER with type a = T.t = struct
    type a = T.t
    module O = Observe.Make(T)
    let capability : a observe_capability = object
      method observe d addr y = Effect.perform (O.Observe (d, addr, y))
    end
    let run tr (Model m) =
      match m capability with
      | ans -> ans
      | effect O.Observe (d, _, y), k ->
          let lp   = Dist.log_prob y d in
          let rest = Effect.Deep.continue k y in
          Stepped { next_particle = rest
                  ; weight        = lp
                  ; trace         = !tr }
  end
end

let advance_linreg (tr0 : Trace.t) (model : (linreg_cap, float * float) model)
    : (float * float) advance_result =
  let module RT = ReuseTrace.Make(struct type t = float end) in
  let module AO = AdvanceObserve.Make(struct type t = float end) in
  let tr = ref tr0 in
  RT.run tr
    (Model (fun scap ->
      AO.run tr
        (Model (fun ocap ->
          let caps : linreg_cap = object
            method sample_float d addr = scap#sample d addr
            method observe_float d addr y = ocap#observe d addr y
          end in
          Finished { value  = run_with_capabilities caps model
                   ; weight = 0.
                   ; trace  = !tr }))))

let advance_hmm (tr0 : Trace.t) (model : (hmm_cap, float * float) model)
    : (float * float) advance_result =
  let module RTF = ReuseTrace.Make(struct type t = float end) in
  let module RTB = ReuseTrace.Make(struct type t = bool end) in
  let module AO  = AdvanceObserve.Make(struct type t = int end) in
  let tr = ref tr0 in
  RTF.run tr
    (Model (fun scap_f ->
      RTB.run tr
        (Model (fun scap_b ->
          AO.run tr
            (Model (fun ocap ->
              let caps : hmm_cap = object
                method sample_float d addr = scap_f#sample d addr
                method sample_bool  d addr = scap_b#sample d addr
                method observe_int  d addr y = ocap#observe d addr y
              end in
              Finished { value  = run_with_capabilities caps model
                       ; weight = 0.
                       ; trace  = !tr }))))))

let advance_latdiri (tr0 : Trace.t) (model : (latdiri_cap, int array) model)
    : int array advance_result =
  let module RTFA = ReuseTrace.Make(struct type t = float array end) in
  let module RTI  = ReuseTrace.Make(struct type t = int end) in
  let module AO   = AdvanceObserve.Make(struct type t = int end) in
  let tr = ref tr0 in
  RTFA.run tr
    (Model (fun scap_fa ->
      RTI.run tr
        (Model (fun scap_i ->
          AO.run tr
            (Model (fun ocap ->
              let caps : latdiri_cap = object
                method sample_float_array d addr = scap_fa#sample d addr
                method sample_int         d addr = scap_i#sample d addr
                method observe_int        d addr y = ocap#observe d addr y
              end in
              Finished { value  = run_with_capabilities caps model
                       ; weight = 0.
                       ; trace  = !tr }))))))


let pf : 'b. int
           -> Trace.t
           -> (Trace.t -> 'b advance_result)
           -> 'b particle list
  = fun n_particles tr0 advance ->
    let init = List.init n_particles (fun _ ->
      { result = advance tr0; weight = 0. }
    ) in
    let rec loop particles =
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
            -> (Trace.t -> 'b advance_result)
            -> 'b particle list
  = fun n_particles advance ->
    match pf n_particles Trace.empty advance with
    | particles -> particles
    | effect (Resample ps), k ->
        Effect.Deep.continue k (resample ps)

let exec_pf : 'b. int
              -> (Trace.t -> 'b advance_result)
              -> Trace.t
              -> 'b * float * Trace.t
  = fun n_particles advance tr ->
    let particles = mpf n_particles advance in
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
      (int_of_float (Rng.next () *. float_of_int (List.length finished))) in
    (value, log_z, trace)

let pmh : 'b. int
            -> int
            -> (Trace.t -> 'b advance_result)
            -> ('b * float * Trace.t) list
  = fun n_mhsteps n_particles advance ->
    match mh n_mhsteps Trace.empty (exec_pf n_particles advance) with
    | chain -> chain
    | effect (Propose tr), k ->
        let addrs   = Trace.addresses tr in
        let n_addrs = List.length addrs in
        if n_addrs = 0 then Effect.Deep.continue k tr
        else
          let i = int_of_float (Rng.next () *. float_of_int n_addrs) in
          let addr = List.nth addrs i in
          let tr'  = Trace.AddrMap.remove addr tr in
          Effect.Deep.continue k tr'
    | effect (Accept (w, w', n, n')), k ->
        let ratio  = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
        Effect.Deep.continue k (ratio > Rng.next ())

let rmpf : 'b. int
             -> int
             -> (Trace.t -> 'b advance_result)
             -> (Trace.t -> 'b * float * Trace.t)
             -> 'b particle list
  = fun n_particles n_mhsteps advance exec ->
    match pf n_particles Trace.empty advance with
    | particles -> particles
    | effect (Resample ps), k ->
        let resampled = resample ps in
        let moved = List.map (fun p ->
          match p.result with
          | Finished _ -> p
          | Stepped s ->
              let chain =
                match mh n_mhsteps s.trace exec with
                | c -> c
                | effect (Propose tr), k ->
                    let addrs   = Trace.addresses tr in
                    let n_addrs = List.length addrs in
                    if n_addrs = 0 then Effect.Deep.continue k tr
                    else
                      let addr = List.nth addrs (int_of_float (Rng.next () *. float_of_int n_addrs)) in
                      Effect.Deep.continue k (Trace.AddrMap.remove addr tr)
                | effect (Accept (w, w', n, n')), k ->
                    let ratio = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
                    Effect.Deep.continue k (ratio > Rng.next ())
              in
              let (_, _, improved_trace) = List.hd chain in
              { p with result = Stepped { s with trace = improved_trace } }
        ) resampled in
        Effect.Deep.continue k moved