open Effect
open Effect.Deep
open Printf

(* Define probabilistic effects *)
type _ Effect.t += Sample : string * float * float -> float Effect.t
type _ Effect.t += Observe : string * float * float * float -> unit Effect.t
type _ Effect.t += Accept : float -> bool Effect.t

type dataset = (float * float) list

(* Linear regression model described as a probabilistic program *)
let lin_reg_model : dataset -> float * float 
  = fun data ->
  let m = perform (Sample ("m", -10., 10.)) in
  let c = perform (Sample ("c", -10., 10.)) in
  List.iter (fun (x, y) ->
    let pred = m *. x +. c in
    perform (Observe ("y", pred -. 1., pred +. 1., y))) data;
  (m, c)

(* Metropolis–Hastings handler (interprets probabilistic effects) *)
let mh_handle : type a. (unit -> a) -> a (* forall a. (() -> a) -> a polymorphic*)
  = fun f ->
  match f () with
  | value -> value
  | effect (Sample (name, low, high)), k ->
      let draw = low +. (Random.float (high -. low)) in
      printf "Sample %s ~ U(%f, %f) = %f\n" name low high draw;
      continue k draw
  | effect (Observe (name, mu, sigma, obs)), k ->
      let within = obs >= mu && obs <= sigma in
      printf "Observe %s -> %b (interval [%f,%f], actual %f)\n" name within mu sigma obs;
      continue k ()
  | effect (Accept p), k ->
      let r = Random.float 1.0 in
      let accepted = r < p in
      printf "Accept? p=%.3f, r=%.3f -> %b\n" p r accepted;
      continue k accepted

(* Proposal step: make small random changes to parameters *)
let propose : float * float -> float * float 
  = fun (m, c) ->
  let step () = (Random.float 1. -. 0.5) in
  (m +. step (), c +. step ())

(* Likelihood of the data given (m, c) parameters *)
let likelihood : dataset -> (float * float) -> float
  = fun data (m, c) ->
  List.fold_left (fun acc (x, y) ->
    let pred = m *. x +. c in
    let diff = y -. pred in
    acc *. exp (-.0.5 *. diff *. diff)) 1.0 data

(* One MH step: propose new parameters, compute acceptance probability *)
let mh_step : dataset -> (float * float) -> float * float
  = fun data (m_prev, c_prev) ->
  let (m_prop, c_prop) = propose (m_prev, c_prev) in
  let l_prev = likelihood data (m_prev, c_prev) in
  let l_prop = likelihood data (m_prop, c_prop) in
  let accept_p = min 1.0 (l_prop /. l_prev) in
  let accepted = mh_handle (fun () -> perform (Accept accept_p)) in
  if accepted then (m_prop, c_prop) else (m_prev, c_prev)

(* Repeat MH steps to form a sampling chain *)
let rec mh_iter : dataset -> int -> (float * float) -> (float * float) list -> (float * float) list
  = fun data n current acc ->
  if n = 0 then List.rev acc
  else
    let next = mh_step data current in
    mh_iter data (n - 1) next (next :: acc)

