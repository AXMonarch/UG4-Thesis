#!/bin/bash

# Generate a file with 50,000,000 random numbers for use by both OCaml and Haskell
echo "Generating rng_sequence.txt with 50,000,000 random numbers..."
cd "$(dirname "$0")"

# Use Rng.generate_and_save function
cat > gen_rng.ml << 'EOF'
let () = 
  Rng.generate_and_save 50_000_000 "rng_sequence.txt";
  print_endline "Generated 50,000,000 random numbers in rng_sequence.txt"
EOF

# Build the library and run the generator
dune build ../../_build/default/lib/ppfxrow/ppfxrow.cma
ocaml -I ../../_build/default/lib/ppfxrow ../../_build/default/lib/ppfxrow/ppfxrow.cma gen_rng.ml
rm gen_rng.ml

echo "Done! File is ready for both OCaml and Haskell benchmarks."
