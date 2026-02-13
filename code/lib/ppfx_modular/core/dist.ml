type t =
  | Uniform    of float * float
  | Normal     of float * float
  | Categorical of float array
  | Beta       of float * float
  | Bernoulli  of float
  | Binomial   of int * float

let draw (dist : t) (u : float) : float =
  match dist with
  | Uniform (a, b) ->
      a +. u *. (b -. a)

  | Normal (mu, sigma) ->
      let u2 = Random.float 1.0 in
      let z  = sqrt (-2.0 *. log u) *. cos (2.0 *. Float.pi *. u2) in
      mu +. sigma *. z

  | Categorical probs ->
      let total = Array.fold_left ( +. ) 0.0 probs in
      let rec go i cumsum =
        if i >= Array.length probs then float_of_int (Array.length probs - 1)
        else
          let cumsum' = cumsum +. probs.(i) /. total in
          if u <= cumsum' then float_of_int i
          else go (i + 1) cumsum'
      in
      go 0 0.0

  | Beta (a, b) ->
      let ga = Random.float 1.0 ** (1.0 /. a) in
      let gb = Random.float 1.0 ** (1.0 /. b) in
      ga /. (ga +. gb)

  | Bernoulli p ->
      if u < p then 1.0 else 0.0

  | Binomial (n, p) ->
      let rec loop i acc =
        if i = 0 then float_of_int acc
        else
          let acc' = acc + (if Random.float 1.0 < p then 1 else 0) in
          loop (i - 1) acc'
      in
      loop n 0

let log_prob (dist : t) (x : float) : float =
  match dist with
  | Uniform (a, b) ->
      if x >= a && x <= b then -. log (b -. a)
      else neg_infinity

  | Normal (mu, sigma) ->
      let d = x -. mu in
      -0.5 *. log (2.0 *. Float.pi *. sigma *. sigma)
      -. (d *. d) /. (2.0 *. sigma *. sigma)

  | Categorical probs ->
      let total = Array.fold_left ( +. ) 0.0 probs in
      let idx   = int_of_float x in
      if idx >= 0 && idx < Array.length probs then
        log (probs.(idx) /. total)
      else neg_infinity

  | Beta (a, b) ->
      if x > 0.0 && x < 1.0 then
        (a -. 1.0) *. log x +. (b -. 1.0) *. log (1.0 -. x)
      else neg_infinity

  | Bernoulli p ->
      if      x = 1.0 then log p
      else if x = 0.0 then log (1.0 -. p)
      else neg_infinity

  | Binomial (n, p) ->
      let k = int_of_float x in
      if k < 0 || k > n then neg_infinity
      else
        float_of_int k *. log p
        +. float_of_int (n - k) *. log (1.0 -. p)