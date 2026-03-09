#!/bin/bash

cd "$(dirname "$0")"

if [ ! -f "rng_sequence.txt" ]; then
    echo "rng_sequence.txt not found. Run ./generate_rng_file.sh first!"
    exit 1
fi

echo "Loading rng_sequence.txt into memory and running benchmarks..."
dune exec ../../_build/default/lib/ppfxrow/pf_bench.exe
