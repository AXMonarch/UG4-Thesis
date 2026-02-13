type address = {
  tag   : string;
  local : int;
}

module AddrMap : Map.S with type key = address

type trace    = float AddrMap.t
type lp_trace = float AddrMap.t

val empty_trace    : trace
val lookup_trace   : address -> trace -> float option
val insert_trace   : address -> float -> trace -> trace
val addresses      : trace -> address list
val trace_size     : trace -> int