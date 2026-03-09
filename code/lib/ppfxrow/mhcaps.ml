[@@@ocaml.warning "-27-32-39"]
open Mh

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

type ('c, 'a) model = Model of ((< .. > as 'c) -> 'a)

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
    (cap : #linreg_cap)
    : float * float =
  let m = cap#sample_float (Dist.normal 0. 3.) (Addr.make ()) in
  let c = cap#sample_float (Dist.normal 0. 2.) (Addr.make ()) in
  List.iter2 (fun x y ->
    let _ = cap#observe_float (Dist.normal (m *. x +. c) 1.) (Addr.make ()) y in
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

(* ---- Demo: running LinReg with layered handlers ---- *)

let linreg_demo () =
  let xs = [0.; 1.; 2.; 3.; 4.] in
  let ys = [1.; 3.; 5.; 7.; 9.] in
  let module SampleHandler = DefaultSample.Make(struct type t = float end) in
  let module ObserveHandler = DefaultObserve.Make(struct type t = float end) in
  SampleHandler.run
    (Model (fun scap ->
      ObserveHandler.run
        (Model (fun ocap ->
          let caps : linreg_cap = object
            method sample_float d addr = scap#sample d addr
            method observe_float d addr y = ocap#observe d addr y
          end in
          run_with_capabilities caps
            (Model (lin_regr_full xs ys))))))