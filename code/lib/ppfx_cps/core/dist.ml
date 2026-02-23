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

  let sqrt_2    = Float.sqrt 2.
  let sqrt_2_pi = Float.sqrt (2. *. Float.pi)
end

type 'a t =
  | Bernoulli : float            -> bool t
  | Normal    : float * float    -> float t
  | Uniform   : float * float    -> float t
  | Beta      : float * float    -> float t
  | Binomial  : int * float      -> int t
  | Categorical : float array    -> int t

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
      let gb = Random.float 1.0 ** (1.0 /. b) in
      ga /. (ga +. gb)
  | Binomial (n, p) ->
      let rec loop i acc =
        if i = 0 then acc
        else loop (i - 1) (acc + (if Random.float 1.0 < p then 1 else 0))
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