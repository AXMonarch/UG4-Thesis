(* AddrMap is a map from addresses to values, 
used to track the choices made during
   a probabilistic program execution. 
   The 'trace' type stores these choices,
   representing the minimal state needed 
   to replay or propose new executions. *)

type address = {
  tag   : string; 
  local : int;
} (* Straight from ProbFX, tag and local pinpoint 
the address that needs to be compared *)


module AddrMap = Map.Make(struct
  type t = address
  let compare a b =
    let c = String.compare a.tag b.tag in
    if c <> 0 then c else Int.compare a.local b.local
end)

(* Compare compares two addresses by tag and local *)
(*Compare two addresses first by their
 tag name. If the names are different, 
 that decides the order. 
 If the names are the same, 
 then compare by the local index to get a 
 unique ordering. *)

(*These trace are never edited by the models or inference algorithms.
 they represent the fixed choices made during execution.
 The perform keyword will summon them from the ordered map.*)

type trace = {
  choices : float AddrMap.t;
}

(** Log probability trace - maps addresses to their log probabilities *)
type lp_trace = float AddrMap.t

(*"Trace" is an input/output trace *)
(* "LP Trace is an output trace"*)
(*Trace is like a cache of random numbers that can be:
Read from (when address exists)
Written to (when new address sampled)
lp_trace is computed fresh every time by:
Recording logProb(dist, value) at each Sample/Observe*)

(*Distributions*)
type distribution =
  | Uniform of float * float
  | Normal of float * float
  | Categorical of float array
  | Beta of float * float
  | Bernoulli of float
  | Binomial of int * float

(* Sample effect: draws a value from a distribution and records it at the given address *)
(* Produces a float result representing the sampled value *)
type _ Effect.t +=
  | Sample  : { addr : address; dist : distribution } -> float Effect.t

(* Observe effect: conditions on an observed value at a given address *)
(* Returns unit; updates the log-probability of the trace *)
type _ Effect.t +=
  | Observe : { addr : address; dist : distribution; obs : float } -> unit Effect.t

(* Propose effect: generates a new candidate trace for Metropolis-Hastings *)
(* Input: current trace *)
(* Output: proposed trace (may modify some/all random choices) *)
type _ Effect.t +=
| Propose : trace -> trace Effect.t

(* Accept effect: performs Metropolis-Hastings acceptance test *)
(* Input: (current_trace, current_log_prob_map), (proposed_trace, proposed_log_prob_map) *)
(* Output: (chosen_trace, chosen_log_prob_map) based on acceptance criterion *)
(* The handler decides whether to accept the proposal or keep the current state *)
type _ Effect.t +=
| Accept : ((trace * lp_trace) * (trace * lp_trace)) -> (trace * lp_trace) Effect.t

(* Resample effect: used in particle filters to choose an index according to weights *)
(* Produces the index of the chosen particle *)
type _ Effect.t +=
  | Resample : float array -> int Effect.t

type 'a particle = {
  trace : trace; 
  weight : float;
  suspended_model : unit -> 'a;
}

(** Create an address from a tag and local index *)
let make_addr (tag : string) (local : int) : address =
  { tag; local }

(** Lookup a random choice in a trace by address *)
let lookup (addr : address) (trace : trace) : float option =
  AddrMap.find_opt addr trace.choices

(** Add or update a random choice in a trace *)
let add_choice (addr : address) (value : float) (trace : trace) : trace =
  { choices = AddrMap.add addr value trace.choices }

(** Create an empty trace *)
let empty_trace () : trace =
  { choices = AddrMap.empty }

(** Get list of all addresses in a trace *)
let addresses (trace : trace) : address list =
  AddrMap.fold (fun addr _ acc -> addr :: acc) trace.choices []

(** ============================================
    Distribution Module
    ============================================ *)

module Dist = struct
  (** Draw a sample from a distribution using a uniform random value *)
  let draw (dist : distribution) (u : float) : float =
    match dist with
    | Uniform (a, b) ->
        a +. (u *. (b -. a))
    
    | Normal (mu, sigma) ->
        (* Box-Muller transform *)
        let u1 = u in
        let u2 = Random.float 1.0 in
        let z = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
        mu +. sigma *. z
    
    | Categorical probs ->
        let total = Array.fold_left (+.) 0.0 probs in
        let rec find_category i cumsum =
          if i >= Array.length probs then
            float_of_int (Array.length probs - 1)
          else
            let new_cumsum = cumsum +. probs.(i) /. total in
            if u <= new_cumsum then float_of_int i
            else find_category (i + 1) new_cumsum
        in
        find_category 0 0.0
      | Beta (a, b) ->
        (* Simple Beta via Gamma trick (placeholder, OK for now) *)
        let ga = Random.float 1.0 ** (1.0 /. a) in
        let gb = Random.float 1.0 ** (1.0 /. b) in
        ga /. (ga +. gb)

      | Bernoulli p ->
        if u < p then 1.0 else 0.0

      | Binomial (n, p) ->
        let rec loop i acc =
          if i = 0 then acc
          else
            let acc' = acc + if Random.float 1.0 < p then 1 else 0 in
            loop (i - 1) acc'
        in
        float_of_int (loop n 0)
  
  (** Compute log probability of a value under a distribution *)
  let log_prob (dist : distribution) (x : float) : float =
    match dist with
    | Uniform (a, b) ->
        if x >= a && x <= b then
          -. log (b -. a)
        else
          neg_infinity
    
    | Normal (mu, sigma) ->
        let diff = x -. mu in
        -0.5 *. log (2.0 *. Float.pi *. sigma *. sigma)
        -. (diff *. diff) /. (2.0 *. sigma *. sigma)
    
    | Categorical probs ->
        let total = Array.fold_left (+.) 0.0 probs in
        let idx = int_of_float x in
        if idx >= 0 && idx < Array.length probs then
          log (probs.(idx) /. total)
        else
          neg_infinity
    | Beta (a, b) ->
        if x > 0.0 && x < 1.0 then
          (a -. 1.0) *. log x
        +. (b -. 1.0) *. log (1.0 -. x)
        else
          neg_infinity

    | Bernoulli p ->
        if x = 1.0 then log p
        else if x = 0.0 then log (1.0 -. p)
        else neg_infinity

    | Binomial (n, p) ->
        let k = int_of_float x in
        if k < 0 || k > n then neg_infinity
        else
          float_of_int k *. log p
        +. float_of_int (n - k) *. log (1.0 -. p)
end