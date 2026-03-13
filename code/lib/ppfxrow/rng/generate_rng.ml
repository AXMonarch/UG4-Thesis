let () =
  Random.init 52;
  let n = 25_000_000 in

  let oc_ml = open_out "rng_data.ml" in
  Printf.fprintf oc_ml "let random_values = [|\n";
  for _ = 1 to n do
    Printf.fprintf oc_ml "  %.17g;\n" (Random.float 1.0)
  done;
  Printf.fprintf oc_ml "|]\n";
  close_out oc_ml;

  Random.init 52;
  let oc_hs = open_out "../../../../prob-fx-2/src/RngData.hs" in
  Printf.fprintf oc_hs "{-# OPTIONS_GHC -O0 #-}\n";
  Printf.fprintf oc_hs "module RngData where\n\n";
  Printf.fprintf oc_hs "import qualified Data.Vector as V\n\n";
  Printf.fprintf oc_hs "randomValues :: V.Vector Double\n";
  Printf.fprintf oc_hs "randomValues = V.fromList\n  [ ";
  for i = 1 to n do
    Printf.fprintf oc_hs "%.17g" (Random.float 1.0);
    if i < n then Printf.fprintf oc_hs "\n  , "
  done;
  Printf.fprintf oc_hs "\n  ]\n";
  close_out oc_hs