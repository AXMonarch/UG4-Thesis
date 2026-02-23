open Effects

let reuse_trace (type a) (type b)
    : a sample_cap -> Trace.t -> (unit, b) model -> b * Trace.t
  = fun (module S) tr0 (Model m) ->
  let tr = ref tr0 in
  match m () with
  | ans -> (ans, !tr)
  | effect (S.Sample (d, addr)), k ->
      let r = Trace.try_insert addr (Random.float 1.0) tr in
      Effect.Deep.continue k (Dist.draw r d)

type ('c, 'w, 'a) model_exec =
  ModelExec of ('c -> Trace.t -> ('c, 'a) model -> 'a * ('w * Trace.t))

let mh (type a) (type b)
    : (b propose_cap * a sample_cap * a observe_cap)
      -> int
      -> Trace.t
      -> (a sample_cap * a observe_cap, float, b) model_exec
      -> (a sample_cap * a observe_cap, b) model
      -> (b * (float * Trace.t)) list
  = fun ((module P), (module S), (module O)) n tr0 (ModelExec exec) m ->
  let rec mh_step i chain =
    if i < n then
      let (x, (w, tr))     = List.hd chain in
      let tr'              = Effect.perform (P.Propose tr) in
      let (x', (w', tr'')) = exec ((module S), (module O)) tr' m in
      let node             = Effect.perform (P.Accept ((x, (w, tr)), (x', (w', tr'')))) in
      mh_step (i + 1) (node :: chain)
    else chain
  in
  let node0 = exec ((module S), (module O)) tr0 m in
  mh_step 0 [node0]

let handle_propose (type a) (type b) (type c)
    : (b propose_cap * a sample_cap * a observe_cap)
      -> (b propose_cap * a sample_cap * a observe_cap, c) model
      -> c
  = fun (((module P), (module S), (module O)) as caps) (Model m) ->
  match m caps with
  | ans -> ans
  | effect P.Propose tr, k ->
      let tr' = Trace.AddrMap.map (fun _ -> Random.float 1.0) tr in
      Effect.Deep.continue k tr'
  | effect P.Accept (((_, (w, _)) as r), ((_, (w', _)) as r')), k ->
      let ratio = w' -. w in
      let u     = Random.float 1.0 in
      Effect.Deep.continue k (if exp ratio >= u then r' else r)


let likelihood (type a) (type b)
    : a observe_cap -> (a observe_cap, b) model -> b * float
  = fun (module O) (Model m) ->
  let w = ref 0. in
  match m (module O) with
  | ans -> (ans, !w)
  | effect O.Observe (d, _, y), k ->
      w := !w +. Dist.log_prob y d;
      Effect.Deep.continue k y


let exec_model_im (type a) (type b)
    : (a sample_cap * a observe_cap, float, b) model_exec
  = ModelExec (fun ((module S), (module O)) tr (Model m) ->
      let ((x, w), tr') =
        reuse_trace (module S) tr
          (Model (fun () ->
            likelihood (module O)
              (Model (fun (module O) -> m ((module S), (module O))))))
      in
      (x, (w, tr')))

let im (type a) (type b)
    : int -> (a sample_cap * a observe_cap, b) model -> (b * (float * Trace.t)) list
  = fun n m ->
  let module Caps = MHCaps.Make(struct
    type t   = a
    type ans = b
  end) in
  handle_propose
    ((module Caps.Propose), (module Caps.Sample), (module Caps.Observe))
    (Model (fun caps -> mh caps n Trace.empty exec_model_im m))