(* Common types for probabilistic models *)

(** A dataset is a list of (input, output) pairs *)
type dataset = (float * float) list

(** Multi-dimensional dataset *)
type dataset_nd = (float list * float) list

(** Time series data *)
type time_series = float array

(** Classification dataset *)
type classification_data = (float list * int) list
