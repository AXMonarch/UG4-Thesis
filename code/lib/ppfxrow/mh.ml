[@@@ocaml.warning "-27-32-39"]
(*Addr, or address, is a data
type that identifies a 
sample site in a model. 
Just an int with a ref
that increments every site*)

module Addr = struct 
  type t = int
  let make =
    let next = ref (-1) in
    fun () ->
      incr next; !next
end

(* Trace is a map between
addresses and a draw from Dist *)

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

(* DIST : A GADT (Generalized Abstract Data type)
for all distributions we need. *)

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
    | Categorical : float array    -> int t
    | Dirichlet  :   float array    -> float array t

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
        let ga = r ** (1.0 /. a) in
        let gb = Rng.next () ** (1.0 /. b) in
        ga /. (ga +. gb)
    | Binomial (n, p) ->
        let rec loop i acc =
          if i = 0 then acc
          else loop (i - 1) (acc + (if Rng.next () < p then 1 else 0))
        in
        loop n 0
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
          float_of_int k *. log p
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

(* Effects : Define ONCE as polymorphic GADTS
 The 'a index on Effect.t carries the return type, so:
Sample (d, addr) : 'a Effect.t   when d : 'a Dist.t
-> Draws a sample from d and maps to addr with trace

Observe (d, x, addr) : unit Effect.t
-> Observes how probable our sample was
-> updates trace with log_prob of x (sampled val.) under d

Propose tr : Trace.t Effect.t
-> proposes a new trace tr' based on tr
-> this is different for IM and SSMH

Accept (w, w', n, n') : bool Effect.t*
-> accepts/rejects based on accepatnce ratio
-> IM doesn't need rest of trace, only n-1 addr
-> SSMH needs both traces to compute acceptance ratio
*)

type _ Effect.t +=
  | Sample  : 'a Dist.t * Addr.t -> 'a Effect.t
  | Observe : 'a Dist.t * 'a * Addr.t -> unit Effect.t
  | Propose : Trace.t -> Trace.t Effect.t
  | Accept  : float * float * int * int -> bool Effect.t

(*sample bool and sample float*)
(*Only 1 effect name, need more, make modules with
different effects inside it.*)
(*[@@ocaml.unboxed] for models *)

(*Capabilty style passing with Objects in OCAML :

First we make model_cap
which is an object type with two polymorphic methods:
- sample : 'a Dist.t -> 'a
- observe : 'a Dist.t -> 'a -> unit

Then we make base_cap, which 
is an object that implements model_cap
by performing the corresponding effects.
 This is what we will pass to our models.

The algorithms can have any number of extensible 
capability fields. Some will ignore 
some of them. 

But the more we have the more stuff that can 
use all of them.

*)

type model_cap = <
  sample  : 'a. 'a Dist.t -> 'a;
  observe : 'a. 'a Dist.t -> 'a -> unit
>

let base_cap : model_cap = object
  method sample : 'a. 'a Dist.t -> 'a 
    = fun d -> Effect.perform (Sample (d, Addr.make ()))
  method observe : 'a. 'a Dist.t -> 'a -> unit 
    = fun d x -> Effect.perform (Observe (d, x, Addr.make ()))
end


(* Function to reuse traces (Open-row style)
Takes model cap and uses it to compare traces*)

let reuse_trace : 'b. Trace.t -> (< sample : 'a. 'a Dist.t -> 'a; .. > -> 'b) -> 'b * Trace.t
  = fun tr0 model ->
    let tr = ref tr0 in
    match model base_cap with
    | ans -> (ans, !tr)
    | effect (Sample (d, addr)), k ->
        let r = Trace.try_insert addr (Rng.next ()) tr in
        Effect.Deep.continue k (Dist.draw r d)


(* Only does Observe effects, 
taking advantage of open rows
Aggregates log_prob across addresses 
which is why _addr
is not needed

This can throw sample effects to the outer handler

Which is why it will sit within reuse trace*)

let likelihood : 'b. (< observe : 'a. 'a Dist.t -> 'a -> unit; .. > -> 'b) -> 'b * float
  = fun model ->
    let w = ref 0. in
    match model base_cap with
    | ans -> (ans, !w)
    | effect (Observe (d, x, _addr)), k ->
        w := !w +. Dist.log_prob x d;
        Effect.Deep.continue k ()

(*What i meant:

reuse trace (manages trace)-> 
  likelihood (manages log weight), inner handler -> 
    model

    reuse trace passes observes to likelihood
    likelihood passes sample to reuse trace
    power of row polymorphism

    Returns output * log_weight * new_trace
*)

let exec_model : 'b. Trace.t -> (< sample : 'a. 'a Dist.t -> 'a; 
                                    observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b) 
                                -> 'b * float * Trace.t
  = fun tr model ->
    let (ans, w), tr' =
      reuse_trace tr (fun cap ->
        likelihood (fun _ ->
          model cap))
    in
    (ans, w, tr')

(* The abstract MH loop. Parametrized by exec which is
   the algorithm's policy for running the model.
   The skeleton has no idea if exec is IM or SSMH.
    
Just like we defered reusetrace
and likelihood to do their effects
here, mh will send propose and accept to the IM
and SSMH handlers.   

The type looks funny :

It takes :
n : iterations

tr0 : initial trace

exec : a function 
that takes a trace 
and a model and returns (output, log_weight, new_trace)

model : any capabilty that has ATLEAST
sample and observe. 

and returns a (output, log_weight, trace) 
for each iteration
*)

let mh : 'b. int 
           -> Trace.t 
           -> (Trace.t -> (< sample : 'a. 'a Dist.t -> 'a; 
                             observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b) 
                       -> 'b * float * Trace.t)
           -> (< sample : 'a. 'a Dist.t -> 'a;
                 observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b)
           -> ('b * float * Trace.t) list
  = fun n tr0 exec model ->
    let rec loop i state chain =
      if i >= n then chain
      else
        let (_, w, tr)    = state in
        let tr'           = Effect.perform (Propose tr) in
        let (x', w', tr'') = exec tr' model in
        let accept        = Effect.perform (Accept (w, w', Trace.size tr, Trace.size tr')) in
        let next          = if accept then (x', w', tr'') else state in
        loop (i + 1) next (next :: chain)
    in
    let init = exec tr0 model in
    loop 0 init [init]


(* IM Handler. 

Things to note : 

Accept is Accept (w, w' _,_)
because we dont care about trace sizes here

Propose is completely independent of current trace
just needs to return a new trace 
with same addresses but different values.

*)
    
let im : 'b. int 
          -> (< sample : 'a. 'a Dist.t -> 'a;
                observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b)
          -> ('b * float * Trace.t) list
  = fun n model ->
    match mh n Trace.empty exec_model model with
    | chain -> chain
    | effect (Propose tr), k ->
        let tr' = Trace.AddrMap.map (fun _ -> Rng.next ()) tr in
        Effect.Deep.continue k tr'
    | effect (Accept (w, w', _, _)), k ->
        let accept = exp (w' -. w) >= Rng.next () in
        Effect.Deep.continue k accept


(* Same shape as IM 
Completely different propose and accept
if no addrs (fresh trace)
  Deep continue with new trace
else
  Propose : randomly select an address and move it
  Accept : compute acceptance ratio
 with log weights and trace sizes
*)

let ssmh : 'b. int
            -> (< sample : 'a. 'a Dist.t -> 'a;
                  observe : 'c. 'c Dist.t -> 'c -> unit; .. > -> 'b)
            -> ('b * float * Trace.t) list
  = fun n model ->
    match mh n Trace.empty exec_model model with
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
        let accept = ratio >= Rng.next () in
        Effect.Deep.continue k accept

(* ALGORITHM CODE DONE *)