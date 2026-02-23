module type SAMPLE = sig
  type a

  type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
end

module Sample = struct
  module Make (T : sig type t end) : SAMPLE with type a = T.t = struct
    type a = T.t

    type _ Effect.t += Sample : a Dist.t * Addr.t -> a Effect.t
  end
end

module type OBSERVE = sig
  type a

  type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
end

module Observe = struct
  module Make (T : sig type t end) : OBSERVE with type a = T.t = struct
    type a = T.t

    type _ Effect.t += Observe : a Dist.t * Addr.t * a -> a Effect.t
  end
end

module type PROPOSE = sig
  type a
  type w = float

  type _ Effect.t +=
    | Propose : Trace.t -> Trace.t Effect.t
    | Accept  : (a * (w * Trace.t)) * (a * (w * Trace.t))
                -> (a * (w * Trace.t)) Effect.t
end

module Propose = struct
  module Make (T : sig type t end) : PROPOSE with type a = T.t = struct
    type a = T.t
    type w = float

    type _ Effect.t +=
      | Propose : Trace.t -> Trace.t Effect.t
      | Accept  : (a * (w * Trace.t)) * (a * (w * Trace.t))
                  -> (a * (w * Trace.t)) Effect.t
  end
end

type 'a sample_cap  = (module SAMPLE  with type a = 'a)
type 'a observe_cap = (module OBSERVE with type a = 'a)
type 'a propose_cap = (module PROPOSE with type a = 'a)

type ('c, 'a) model = Model of ('c -> 'a)

module MHCaps = struct
  module Make (M : sig type t type ans end) = struct
    module Sample  : SAMPLE  with type a = M.t   = Sample.Make  (struct type t = M.t   end)
    module Observe : OBSERVE with type a = M.t   = Observe.Make (struct type t = M.t   end)
    module Propose : PROPOSE with type a = M.ans = Propose.Make (struct type t = M.ans end)
  end
end