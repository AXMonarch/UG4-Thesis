let mean xs =
  let sum = List.fold_left ( +. ) 0.0 xs in
  let n = float_of_int (List.length xs) in
  if n = 0.0 then nan else sum /. n

let variance xs =
  let n = float_of_int (List.length xs) in
  if n = 0.0 then nan
  else
    let m = mean xs in
    let sum_sq = List.fold_left (fun acc x -> acc +. (x -. m) ** 2.) 0.0 xs in
    sum_sq /. n

let std_dev xs =
  sqrt (variance xs)

let uniform a b =
  a +. Random.float (b -. a)

let log_sum_exp xs =
  let max_x = List.fold_left max neg_infinity xs in
  let sum_exp = List.fold_left (fun acc x -> acc +. exp (x -. max_x)) 0.0 xs in
  max_x +. log sum_exp

let normalize_weights ws =
  let sum = Array.fold_left ( +. ) 0.0 ws in
  if sum = 0.0 then ws
  else Array.map (fun w -> w /. sum) ws

let categorical weights =
  let normalized = normalize_weights weights in
  let r = Random.float 1.0 in
  let rec aux i acc =
    if i >= Array.length normalized then Array.length normalized - 1
    else
      let acc = acc +. normalized.(i) in
      if r <= acc then i else aux (i + 1) acc
  in
  aux 0 0.0