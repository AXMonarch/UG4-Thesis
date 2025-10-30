open Effect
open Effects
open Types

let hidden_markov_model (num_states : int) (observations : time_series) : int list =
  let rec sample_states t states =
    if t >= Array.length observations then List.rev states
    else
      let prev_state = if states = [] then 0 else List.hd states in
      let state =
        int_of_float
          (perform
             (Sample
                {
                  name = "state_" ^ string_of_int t;
                  dist = Uniform (0.0, float_of_int num_states);
                }))
      in
      let obs = observations.(t) in
      let emission_mean = float_of_int (prev_state + state) in
      perform
        (Observe
           {
             name = "obs_" ^ string_of_int t;
             dist = Normal (emission_mean, 1.0);
             obs;
           });
      sample_states (t + 1) (state :: states)
  in
  sample_states 0 []
