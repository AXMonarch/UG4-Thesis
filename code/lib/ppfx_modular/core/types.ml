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
type lp_trace = float AddrMap.t

(* Extended with obs_idx to track observation progress      *)
(* obs_idx threads what the paper threads through handler   *)
(* state t in handleResamplermpf                            *)
type pstate = {
  log_weight : float;
  trace      : trace;
  obs_idx    : int;
}

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

let empty_pstate : pstate = {
  log_weight = 0.0;
  trace      = empty_trace;
  obs_idx    = 0;
}