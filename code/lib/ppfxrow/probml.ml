[@@@ocaml.warning "-27-32-39"]

(* ============================================================
   ADDR
   ============================================================ *)

module Addr = struct
  type t = int
  let make =
    let next = ref (-1) in
    fun () ->
      incr next; !next
end

(* ============================================================
   TRACE
   ============================================================ *)

module Trace = struct
  module AddrMap = Map.Make(Int)
  type t = float AddrMap.t

  let empty : t = AddrMap.empty

  let try_insert : Addr.t -> float -> t ref -> float
    = fun addr r tr ->
      match AddrMap.find_opt addr !tr with
      | None    -> tr := AddrMap.add addr r !tr; r
      | Some r' -> r'

  let addresses : t -> Addr.t list
    = fun tr ->
      AddrMap.fold (fun addr _ acc -> addr :: acc) tr []

  let size : t -> int = AddrMap.cardinal
end

(* ============================================================
   DIST
   ============================================================ *)

module Dist = struct
  module Utils = struct
    let neg_inf = -. 1. /. 0.
    let pos_inf = 1. /. 0.

    let inv_erfc : float -> float
      = fun y ->
      if Float.compare y 0. <= 0 then pos_inf
      else if Float.compare y 2. >= 1 then neg_inf
      else
        let x = 1.0 -. y in
        let w = -. log ((1.0 -. x) *. (1.0 +. x)) in
        let p =
          if w < 5.0 then
            let w = w -. 2.5 in
            let p = 2.81022636e-08 in
            let p = 3.43273939e-07 +. p *. w in
            let p = -.3.5233877e-06 +. p *. w in
            let p = -.4.39150654e-06 +. p *. w in
            let p = 0.00021858087 +. p *. w in
            let p = -.0.00125372503 +. p *. w in
            let p = -.0.00417768164 +. p *. w in
            let p = 0.246640727 +. p *. w in
            1.50140941 +. p *. w
          else
            let w = sqrt w -. 3.0 in
            let p = -.0.000200214257 in
            let p = 0.000100950558 +. p *. w in
            let p = 0.00134934322 +. p *. w in
            let p = -.0.00367342844 +. p *. w in
            let p = 0.00573950773 +. p *. w in
            let p = -.0.0076224613 +. p *. w in
            let p = 0.00943887047 +. p *. w in
            let p = 1.00167406 +. p *. w in
            2.83297682 +. p *. w
        in
        p *. x

    let lcg_next (n : int) : int =
      (1664525 * n + 1013904223) mod 2147483647

    let lcg_expand (r : float) (k : int) : float array =
      let seed = int_of_float (r *. 1e16) mod 2147483647 in
      let rs = Array.make k 0. in
      let cur = ref (lcg_next seed) in
      for i = 0 to k - 1 do
        rs.(i) <- float_of_int !cur /. 2147483647.;
        cur := lcg_next !cur
      done;
      rs

    let sqrt_2    = Float.sqrt 2.
    let sqrt_2_pi = Float.sqrt (2. *. Float.pi)
  end

  type 'a t =
    | Bernoulli :   float            -> bool t
    | Normal    :   float * float    -> float t
    | Uniform   :   float * float    -> float t
    | Beta      :   float * float    -> float t
    | Binomial  :   int * float      -> int t
    | Categorical : float array      -> int t
    | Dirichlet  :   float array     -> float array t

  let bernoulli : float -> bool t
    = fun p ->
    assert (p >= 0. && p <= 1.);
    Bernoulli p

  let normal : float -> float -> float t
    = fun mu sigma ->
    assert (sigma > 0.);
    Normal (mu, sigma)

  let uniform : float -> float -> float t
    = fun a b ->
    assert (a < b);
    Uniform (a, b)

  let beta : float -> float -> float t
    = fun a b ->
    assert (a > 0. && b > 0.);
    Beta (a, b)

  let binomial : int -> float -> int t
    = fun n p ->
    assert (n >= 0 && p >= 0. && p <= 1.);
    Binomial (n, p)

  let categorical : float array -> int t
    = fun probs ->
    Categorical probs

  let dirichlet : float array -> float array t
    = fun alphas ->
    assert (Array.length alphas >= 2);
    assert (Array.for_all (fun a -> a > 0.) alphas);
    Dirichlet alphas

  let draw : type a. float -> a t -> a
    = fun r d ->
    match d with
    | Bernoulli p -> r <= p
    | Normal (mu, sigma) ->
        (-. Utils.inv_erfc (2. *. r)) *. (Utils.sqrt_2 *. sigma) +. mu
    | Uniform (a, b) ->
        a +. r *. (b -. a)
    | Beta (a, b) ->
        let rs = Utils.lcg_expand r 2 in
        let ga = rs.(0) ** (1.0 /. a) in
        let gb = rs.(1) ** (1.0 /. b) in
        ga /. (ga +. gb)
    | Binomial (n, p) ->
        let rs = Utils.lcg_expand r n in
        Array.fold_left (fun acc ri -> acc + (if ri < p then 1 else 0)) 0 rs
    | Categorical probs ->
        let total = Array.fold_left ( +. ) 0.0 probs in
        let rec go i cumsum =
          if i >= Array.length probs then Array.length probs - 1
          else
            let cumsum' = cumsum +. probs.(i) /. total in
            if r <= cumsum' then i
            else go (i + 1) cumsum'
        in
        go 0 0.0
    | Dirichlet alphas ->
        let k = Array.length alphas in
        let rs = Utils.lcg_expand r k in
        let xs = Array.map2 (fun a ri -> ri ** (1.0 /. a)) alphas rs in
        let total = Array.fold_left ( +. ) 0. xs in
        Array.map (fun x -> x /. total) xs

  let log_prob : type a. a -> a t -> float
    = fun x d ->
    match d with
    | Bernoulli p ->
        if x then log p else log (1. -. p)
    | Normal (mu, sigma) ->
        let x_mu = x -. mu in
        -. (x_mu *. x_mu /. (2. *. sigma *. sigma))
        -. log Utils.sqrt_2_pi -. log sigma
    | Uniform (a, b) ->
        if x >= a && x <= b then -. log (b -. a) else neg_infinity
    | Beta (a, b) ->
        if x > 0. && x < 1. then
          (a -. 1.) *. log x +. (b -. 1.) *. log (1. -. x)
        else neg_infinity
    | Binomial (n, p) ->
        let k = x in
        if k < 0 || k > n then neg_infinity
        else
          let log_choose n k =
            let rec log_fact n = if n <= 1 then 0. else log (float_of_int n) +. log_fact (n-1) in
            log_fact n -. log_fact k -. log_fact (n - k)
          in
          log_choose n k
          +. float_of_int k *. log p
          +. float_of_int (n - k) *. log (1. -. p)
    | Categorical probs ->
        let total = Array.fold_left ( +. ) 0.0 probs in
        if x >= 0 && x < Array.length probs then
          log (probs.(x) /. total)
        else neg_infinity
    | Dirichlet alphas ->
        let log_gamma a =
          if a <= 0. then neg_infinity
          else (a -. 0.5) *. log a -. a +. 0.5 *. log (2. *. Float.pi)
        in
        let sum_a = Array.fold_left ( +. ) 0. alphas in
        let n = Array.length alphas in
        let lp = ref (log_gamma sum_a
          -. Array.fold_left (fun acc a -> acc +. log_gamma a) 0. alphas) in
        for i = 0 to n - 1 do
          lp := !lp +. (alphas.(i) -. 1.) *. log x.(i)
        done;
        !lp
end

(* ============================================================
   PARTICLE FILTER TYPES  (was: pf.ml)
   ============================================================ *)

type 'a advance_result =
  | Finished of { value : 'a; weight : float; trace : Trace.t }
  | Stepped  of { next_particle : 'a advance_result; weight : float; trace : Trace.t }

type 'a particle = { result : 'a advance_result; weight : float }

(* ============================================================
   CAPABILITY INFRASTRUCTURE
   ============================================================ *)

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

class type observe_bool_cap = object
  method observe_bool : bool Dist.t -> Addr.t -> bool -> bool
end

class type observe_int_cap = object
  method observe_int : int Dist.t -> Addr.t -> int -> int
end

(* ---- Model capability compositions ---- *)

class type coin_flip_cap = object
  inherit sample_float_cap
  inherit observe_bool_cap
end

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
          let r = Rng.next() in
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
module ObserveBool      = Observe.Make(struct type t = bool end)
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

class observe_bool_class : observe_bool_cap = object
  method observe_bool d addr x = Effect.perform (ObserveBool.Observe (d, addr, x))
end

class observe_int_class : observe_int_cap = object
  method observe_int d addr x = Effect.perform (ObserveInt.Observe (d, addr, x))
end

(* ---- Composite base caps ---- *)

let coin_flip_base_cap : coin_flip_cap = object
  inherit sample_float_class
  inherit observe_bool_class
end

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

(* ============================================================
   MODELS
   ============================================================ *)

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
  let trans_p = cap#sample_float (Dist.beta 1. 4.) (Addr.make ()) in
  let obs_p   = cap#sample_float (Dist.beta 1. 1.) (Addr.make ()) in
  let x = ref x0 in
  for i = 0 to n - 1 do
    let dx = if Dist.draw (Rng.next ()) (Dist.bernoulli trans_p) then 1 else 0 in
    x := !x + dx;
    let _ = cap#observe_int (Dist.binomial !x obs_p) (Addr.make ()) ys.(i) in
    ()
  done;
  (trans_p, obs_p)

let vocab_size = 4
let n_topics   = 2

let vocab = [| "DNA"; "evolution"; "parsing"; "phonology" |]

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
    let z = Dist.draw (Rng.next ()) (Dist.categorical doc_topic_ps) in
    let word_ps = topic_word_ps.(z) in
    let word_ps_zipped = Array.map2 (fun _w p -> p) vocab word_ps in
    cap#observe_int
      (Dist.categorical word_ps_zipped)
      (Addr.make ())
      obs.(i)
  )

(* ============================================================
   INFERENCE HANDLER FUNCTORS
   ============================================================ *)

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

(* ============================================================
   EXEC FUNCTIONS
   ============================================================ *)

let exec_model_coin_flip (tr0 : Trace.t)
    (model : (coin_flip_cap, float) model)
    : float * float * Trace.t =
  let module RT = ReuseTrace.Make(struct type t = float end) in
  let module LH = Likelihood.Make(struct type t = bool  end) in
  let tr = ref tr0 in
  let (ans, w) =
    RT.run tr
      (Model (fun scap ->
        LH.run
          (Model (fun ocap ->
            let caps : coin_flip_cap = object
              method sample_float d addr   = scap#sample d addr
              method observe_bool d addr y = ocap#observe d addr y
            end in
            run_with_capabilities caps model))))
  in
  (ans, w, !tr)

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

let exec_pf_linreg (tr0 : Trace.t) (model : (linreg_cap, float * float) model)
    : (float * float) advance_result =
  let module RT = ReuseTrace.Make(struct type t = float end) in
  let module AO = AdvanceObserve.Make(struct type t = float end) in
  let tr = ref tr0 in
  AO.run tr
    (Model (fun ocap ->
      RT.run tr
        (Model (fun scap ->
          let caps : linreg_cap = object
            method sample_float  d addr   = scap#sample d addr
            method observe_float d addr y = ocap#observe d addr y
          end in
          Finished { value  = run_with_capabilities caps model
                   ; weight = 0.
                   ; trace  = !tr }))))

let exec_pf_hmm (tr0 : Trace.t) (model : (hmm_cap, float * float) model)
    : (float * float) advance_result =
  let module RTF = ReuseTrace.Make(struct type t = float end) in
  let module DSB = DefaultSample.Make(struct type t = bool end) in
  let module AO  = AdvanceObserve.Make(struct type t = int end) in
  let tr = ref tr0 in
  AO.run tr
    (Model (fun ocap ->
      RTF.run tr
        (Model (fun scap_f ->
          DSB.run
            (Model (fun scap_b ->
              let caps : hmm_cap = object
                method sample_float d addr   = scap_f#sample d addr
                method sample_bool  d addr   = scap_b#sample d addr
                method observe_int  d addr y = ocap#observe d addr y
              end in
              Finished { value  = run_with_capabilities caps model
                       ; weight = 0.
                       ; trace  = !tr }))))))

let exec_pf_latdiri (tr0 : Trace.t) (model : (latdiri_cap, int array) model)
    : int array advance_result =
  let module RTFA = ReuseTrace.Make(struct type t = float array end) in
  let module RTI  = ReuseTrace.Make(struct type t = int end) in
  let module AO   = AdvanceObserve.Make(struct type t = int end) in
  let tr = ref tr0 in
  AO.run tr
    (Model (fun ocap ->
      RTFA.run tr
        (Model (fun scap_fa ->
          RTI.run tr
            (Model (fun scap_i ->
              let caps : latdiri_cap = object
                method sample_float_array d addr = scap_fa#sample d addr
                method sample_int         d addr = scap_i#sample d addr
                method observe_int        d addr y = ocap#observe d addr y
              end in
              Finished { value  = run_with_capabilities caps model
                       ; weight = 0.
                       ; trace  = !tr }))))))

let exec_pf_coin_flip (tr0 : Trace.t) (model : (coin_flip_cap, float) model)
    : float advance_result =
  let module RT = ReuseTrace.Make(struct type t = float end) in
  let module AO = AdvanceObserve.Make(struct type t = bool end) in
  let tr = ref tr0 in
  AO.run tr
    (Model (fun ocap ->
      RT.run tr
        (Model (fun scap ->
          let caps : coin_flip_cap = object
            method sample_float d addr   = scap#sample d addr
            method observe_bool d addr y = ocap#observe d addr y
          end in
          Finished { value  = run_with_capabilities caps model
                   ; weight = 0.
                   ; trace  = !tr }))))

(* ============================================================
   MH INFERENCE
   ============================================================ *)

module type PROPOSE_ACCEPT = sig
  type a
  type w
  type _ Effect.t += Propose : Trace.t -> Trace.t Effect.t
  type _ Effect.t += Accept  : (a * w * Trace.t) * (a * w * Trace.t)
                             -> (a * w * Trace.t) Effect.t
end

module Propose_Accept = struct
  module Make(A : R)(W : R) : PROPOSE_ACCEPT with type a = A.t and type w = W.t = struct
    type a = A.t
    type w = W.t
    type _ Effect.t += Propose : Trace.t -> Trace.t Effect.t
    type _ Effect.t += Accept  : (a * w * Trace.t) * (a * w * Trace.t)
                               -> (a * w * Trace.t) Effect.t
  end
end

class type propose_cap = object
  method propose : Trace.t -> Trace.t
end

class type ['a] accept_cap = object
  method accept : ('a * float * Trace.t)
               -> ('a * float * Trace.t)
               -> ('a * float * Trace.t)
end

let generic_mh (type a)
    n tr0
    (exec    : Trace.t -> a * float * Trace.t)
    (pcap    : propose_cap)
    (acap    : a accept_cap)
    : (a * float * Trace.t) list =
  let rec loop i state chain =
    if i >= n then chain
    else
      let (_, _, tr)     = state in
      let tr'            = pcap#propose tr in
      let (x', w', tr'') = exec tr' in
      let next           = acap#accept state (x', w', tr'') in
      loop (i + 1) next (next :: chain)
  in
  let init = exec tr0 in
  loop 0 init [init]

(* ---- IM handler — constructs IM caps via Propose module ---- *)

module type IM_HANDLER = sig
  type a
  val run : (propose_cap -> a accept_cap -> (a * float * Trace.t) list)
          -> (a * float * Trace.t) list
end

module ImHandler = struct
  module Make(A : R) : IM_HANDLER with type a = A.t = struct
    type a = A.t
    module P = Propose_Accept.Make(A)(struct type t = float end)
    let run f =
      let pcap = object
        method propose tr = Effect.perform (P.Propose tr)
      end in
      let acap = object
        method accept current proposed = Effect.perform (P.Accept (current, proposed))
      end in
      match f pcap acap with
      | ans -> ans
      | effect P.Propose tr, k ->
          let tr' = Trace.AddrMap.map (fun _ -> Rng.next ()) tr in
          Effect.Deep.continue k tr'
      | effect P.Accept (current, proposed), k ->
          let (_, w,  _) = current in
          let (_, w', _) = proposed in
          let next = if exp (w' -. w) > Rng.next () then proposed else current in
          Effect.Deep.continue k next
  end
end

(* ---- SSMH handler — constructs SSMH caps via Propose module ---- *)

module type SSMH_HANDLER = sig
  type a
  val run : (propose_cap -> a accept_cap -> (a * float * Trace.t) list)
          -> (a * float * Trace.t) list
end

module SsmhHandler = struct
  module Make(A : R) : SSMH_HANDLER with type a = A.t = struct
    type a = A.t
    module P = Propose_Accept.Make(A)(struct type t = float end)
    let run f =
      let pcap = object
        method propose tr = Effect.perform (P.Propose tr)
      end in
      let acap = object
        method accept current proposed = Effect.perform (P.Accept (current, proposed))
      end in
      match f pcap acap with
      | ans -> ans
      | effect P.Propose tr, k ->
          let addrs   = Trace.addresses tr in
          let n_addrs = List.length addrs in
          if n_addrs = 0 then Effect.Deep.continue k tr
          else
            let i    = int_of_float (Rng.next () *. float_of_int n_addrs) in
            let addr = List.nth addrs i in
            let tr'  = Trace.AddrMap.add addr (Rng.next ()) tr in
            Effect.Deep.continue k tr'
      | effect P.Accept (current, proposed), k ->
          let (_, w,  _) = current in
          let (_, w', _) = proposed in
          let n   = Trace.size (let (_,_,t) = current  in t) in
          let n'  = Trace.size (let (_,_,t) = proposed in t) in
          let ratio = exp (w' -. w) *. (float_of_int n /. float_of_int n') in
          let next = if ratio > Rng.next () then proposed else current in
          Effect.Deep.continue k next
  end
end

let imh_eff (type a) n (exec : Trace.t -> a * float * Trace.t) =
  let module IMH = ImHandler.Make(struct type t = a end) in
  IMH.run (fun pcap acap -> generic_mh n Trace.empty exec pcap acap)

let ssmh_eff (type a) n (exec : Trace.t -> a * float * Trace.t) =
  let module SSMH = SsmhHandler.Make(struct type t = a end) in
  SSMH.run (fun pcap acap -> generic_mh n Trace.empty exec pcap acap)

(* ============================================================
   PF INFERENCE
   ============================================================ *)

module type RESAMPLE = sig
  type a
  type _ Effect.t += Resample : a particle list -> a particle list Effect.t
end

module Resample = struct
  module Make(T : R) : RESAMPLE with type a = T.t = struct
    type a = T.t
    type _ Effect.t += Resample : a particle list -> a particle list Effect.t
  end
end

class type ['a] resample_cap = object
  method resample : 'a particle list -> 'a particle list
end

let multinomial_resample : 'a particle list -> 'a particle list
  = fun particles ->
    let n       = List.length particles in
    let weights = List.map (fun p -> p.weight) particles in
    let max_w   = List.fold_left max neg_infinity weights in
    let scaled  = List.map (fun w -> exp (w -. max_w)) weights in
    let total   = List.fold_left ( +. ) 0. scaled in
    let norm_ws = Array.of_list (List.map (fun w -> w /. total) scaled) in
    let ps      = Array.of_list particles in
    List.init n (fun _ ->
      let idx = Dist.draw (Rng.next ()) (Dist.categorical norm_ws) in
      { ps.(idx) with weight = 0. })

let generic_pf (type a)
    n tr0
    (advance : Trace.t -> a advance_result)
    (rcap    : a resample_cap)
    : a particle list =
  let init = List.init n (fun _ ->
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
      let resampled = rcap#resample weighted in
      let stepped = List.map (fun p ->
        match p.result with
        | Stepped  s -> { p with result = s.next_particle }
        | Finished _ -> p
      ) resampled in
      loop stepped
    end
  in
  loop init

(* ---- Multinomial handler — constructs resample cap via Resample module ---- *)

module type MULTINOMIAL_HANDLER = sig
  type a
  val run : (a resample_cap -> a particle list) -> a particle list
end

module MultinomialHandler = struct
  module Make(T : R) : MULTINOMIAL_HANDLER with type a = T.t = struct
    type a = T.t
    module RS = Resample.Make(T)
    let run f =
      let rcap = object
        method resample ps = Effect.perform (RS.Resample ps)
      end in
      match f rcap with
      | ans -> ans
      | effect RS.Resample ps, k ->
          Effect.Deep.continue k (multinomial_resample ps)
  end
end

let mpf_eff (type a) n (advance : Trace.t -> a advance_result) =
  let module MPF = MultinomialHandler.Make(struct type t = a end) in
  MPF.run (fun rcap -> generic_pf n Trace.empty advance rcap)

(* ============================================================
   HYBRID INFERENCE
   ============================================================ *)

let handle_pmh (type a) n_particles (advance : Trace.t -> a advance_result) tr
    : a * float * Trace.t =
  let module PMH = MultinomialHandler.Make(struct type t = a end) in
  let particles = PMH.run (fun rcap ->
    generic_pf n_particles tr advance rcap) in
  let finished = List.filter_map (fun p ->
    match p.result with
    | Finished f -> Some (f.value, p.weight, f.trace)
    | Stepped  _ -> None
  ) particles in
  let weights = List.map (fun (_, w, _) -> w) finished in
  let max_w   = List.fold_left max neg_infinity weights in
  let log_z   = log (List.fold_left (fun acc w ->
      acc +. exp (w -. max_w)) 0. weights)
    +. max_w -. log (float_of_int n_particles) in
  let sum     = List.fold_left (fun acc w -> acc +. exp (w -. max_w)) 0. weights in
  let norm_ws = Array.of_list (List.map (fun w -> exp (w -. max_w) /. sum) weights) in
  let idx     = Dist.draw (Rng.next ()) (Dist.categorical norm_ws) in
  let (value, _, trace) = List.nth finished idx in
  (value, log_z, trace)

let pmh_eff (type a) n_mhsteps n_particles
    (advance : Trace.t -> a advance_result)
    (exec : Trace.t -> a * float * Trace.t) =
  let module IMH = ImHandler.Make(struct type t = a end) in
  let (_, _, tr_init) = exec Trace.empty in
  IMH.run (fun pcap acap ->
    generic_mh n_mhsteps tr_init
      (handle_pmh n_particles advance) pcap acap)

(* ---- Move handler — multinomial resample + SSMH rejuvenation ---- *)

module type MOVE_HANDLER = sig
  type a
  val run : int
          -> (Trace.t -> a * float * Trace.t)
          -> (a resample_cap -> a particle list)
          -> a particle list
end

module MoveHandler = struct
  module Make(T : R) : MOVE_HANDLER with type a = T.t = struct
    type a = T.t
    module RS = Resample.Make(T)
    let run n_mhsteps exec f =
      let module SSMH = SsmhHandler.Make(T) in
      let rcap = object
        method resample ps = Effect.perform (RS.Resample ps)
      end in
      match f rcap with
      | ans -> ans
      | effect RS.Resample ps, k ->
          let resampled = multinomial_resample ps in
          let moved = List.map (fun p ->
            match p.result with
            | Finished _ -> p
            | Stepped s  ->
                let chain = SSMH.run (fun pcap acap ->
                  generic_mh n_mhsteps s.trace exec pcap acap) in
                let (_, _, improved_trace) = List.hd chain in
                { p with result = Stepped { s with trace = improved_trace } }
          ) resampled in
          Effect.Deep.continue k moved
  end
end

let rmpf_eff (type a) n_particles n_mhsteps
    (advance : Trace.t -> a advance_result)
    (exec : Trace.t -> a * float * Trace.t) =
  let module RMPF = MoveHandler.Make(struct type t = a end) in
  RMPF.run n_mhsteps exec (fun rcap ->
    generic_pf n_particles Trace.empty advance rcap)

(* ALTERNATE PF WITH NEW TYPES AND ALGOS *)

type _ Effect.t += Suspend : float -> unit Effect.t

type 'a pf_particle =
  | PF_Done      of { value : 'a; weight : float; trace : Trace.t }
  | PF_Suspended of { resume : unit -> 'a pf_particle; weight : float; trace : Trace.t }

let advance (weight : float) (tr : Trace.t ref) (f : unit -> 'a) : 'a pf_particle =
  match f () with
  | result ->
      PF_Done { value = result; weight; trace = !tr }
  | effect (Suspend lp), k ->
      let w' = weight +. lp in
      PF_Suspended
        { weight = w'
        ; trace  = !tr
        ; resume = fun () -> Effect.Deep.continue k ()
        }

class observe_int_suspend_class : observe_int_cap = object
  method observe_int d addr y =
    let lp = Dist.log_prob y d in
    Effect.perform (Suspend lp);
    y
end

class observe_float_suspend_class : observe_float_cap = object
  method observe_float d addr y =
    let lp = Dist.log_prob y d in
    Effect.perform (Suspend lp);
    y
end

class observe_int_skip_class (skip : int) : observe_int_cap = object
  val mutable count = 0
  method observe_int d addr y =
    let lp = Dist.log_prob y d in
    count <- count + 1;
    if count <= skip then y
    else (Effect.perform (Suspend lp); y)
end

(* ---- MPF: Multinomial Particle Filter ---- *)

let pf_multinomial_resample (particles : 'a pf_particle array) : 'a pf_particle array =
  let weights = Array.map (fun p -> match p with
    | PF_Done      { weight; _ } -> weight
    | PF_Suspended { weight; _ } -> weight
  ) particles in
  let max_w  = Array.fold_left max neg_infinity weights in
  let scaled = Array.map (fun w -> exp (w -. max_w)) weights in
  let total  = Array.fold_left (+.) 0. scaled in
  let norm   = Array.map (fun w -> w /. total) scaled in
  Array.init (Array.length particles) (fun _ ->
    let idx = Dist.draw (Rng.next ()) (Dist.categorical norm) in
    particles.(idx)
  )

let mpf_new (type a) (n_particles : int)
    (model : #hmm_cap -> a)
    : a pf_particle array =
  let run_particle () =
  let tr = ref Trace.empty in
  let cap = object
    method sample_float d _addr = Dist.draw (Rng.next ()) d
    method sample_bool  d _addr = Dist.draw (Rng.next ()) d
    method observe_int  d addr y =
      let lp = Dist.log_prob y d in
      Effect.perform (Suspend lp); y
  end in
  advance 0. tr (fun () -> model cap)
  in
  let particles = Array.init n_particles (fun _ -> run_particle ()) in
  let rec loop ps =
    (* step first *)
    let stepped = Array.map (fun p -> match p with
      | PF_Done _ -> p
      | PF_Suspended { resume; _ } -> resume ()
    ) ps in
    if Array.for_all (fun p -> match p with PF_Done _ -> true | _ -> false) stepped
    then stepped
    else
      let resampled = pf_multinomial_resample stepped in
      loop resampled
  in
  loop particles