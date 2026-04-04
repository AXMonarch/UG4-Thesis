let () =
  let n = 22_557_000 in
  if not (Sys.file_exists "rng_data.ml") then begin
    Random.init 52;
    let oc_ml = open_out "rng_data.ml" in
    Printf.fprintf oc_ml "let random_values = [|\n";
    for _ = 1 to n do
      Printf.fprintf oc_ml "  %.17g;\n" (Random.float 1.0)
    done;
    Printf.fprintf oc_ml "|]\n";
    close_out oc_ml
  end;
  if not (Sys.file_exists "../../../../prob-fx-2/src/rng_data.bin") then begin
    Random.init 52;
    let oc_bin = open_out_bin "../../../../prob-fx-2/src/rng_data.bin" in
    for _ = 1 to n do
      let v = Random.float 1.0 in
      let bits = Int64.bits_of_float v in
      for byte = 7 downto 0 do
        output_byte oc_bin
          (Int64.to_int (Int64.shift_right_logical bits (byte * 8)) land 0xFF)
      done
    done;
    close_out oc_bin
  end