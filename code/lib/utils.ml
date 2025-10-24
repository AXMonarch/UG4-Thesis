(* Helper: arithmetic mean *)
let mean : float list -> float 
  = fun lst ->
  let sum = List.fold_left ( +. ) 0.0 lst in
  sum /. float_of_int (List.length lst)