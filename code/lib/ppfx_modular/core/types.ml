(* Fig 7: Trace = Map Addr Double *)
(* Fig 9: LPTrace = Map Addr LogP  *)

type address = {
  tag   : string;
  local : int;
}

module AddrMap = Map.Make(struct
  type t = address
  let compare a b =
    let c = String.compare a.tag b.tag in
    if c <> 0 then c else Int.compare a.local b.local
end)

type trace    = float AddrMap.t
(* paper: type Trace = Map Addr Double        *)
(* a map from addresses to raw uniform values *)

type lp_trace = float AddrMap.t
(* paper: type LPTrace = Map Addr LogP        *)
(* a map from addresses to log probabilities  *)

let empty_trace : trace = AddrMap.empty

let lookup_trace (addr : address) (t : trace) : float option =
  AddrMap.find_opt addr t

let insert_trace (addr : address) (v : float) (t : trace) : trace =
  AddrMap.add addr v t

let addresses (t : trace) : address list =
  AddrMap.fold (fun addr _ acc -> addr :: acc) t []

let trace_size (t : trace) : int =
  AddrMap.cardinal t

let make_addr (tag : string) (local : int) : address =
  { tag; local }