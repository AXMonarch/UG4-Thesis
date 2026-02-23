module AddrMap = Map.Make (struct
  type t = Addr.t
  let compare = Int.compare
end)

type t = float AddrMap.t

let try_insert : Addr.t -> float -> t ref -> float
  = fun addr r tr ->
  match AddrMap.find_opt addr !tr with
  | None   -> tr := AddrMap.add addr r !tr; r
  | Some r' -> r'

let empty : t = AddrMap.empty

let addresses : t -> Addr.t list
  = fun tr ->
  AddrMap.fold (fun addr _ acc -> addr :: acc) tr []

let size : t -> int
  = fun tr ->
  AddrMap.cardinal tr

let pp : Format.formatter -> t -> unit
  = fun fmt tr ->
  let n   = AddrMap.cardinal tr in
  let cur = ref 0 in
  Format.fprintf fmt "{";
  AddrMap.iter (fun addr value ->
    Format.fprintf fmt "%d -> %f" addr value;
    incr cur;
    if !cur < n then Format.fprintf fmt ", ") tr;
  Format.fprintf fmt "}"