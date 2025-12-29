#!/bin/bash

# Example script for running comprehensive benchmarks
# This script demonstrates different benchmark configurations

echo "Running comprehensive benchmarks..."
echo ""

# Create output directory
mkdir -p benchmark_results

# MH benchmarks with varying iterations
echo "1. MH benchmarks with varying iterations..."
dune exec bench_cli -- -alg mh -seqlens 10,20,50,100 -particles 50,100,200,500 -repeats 10 > benchmark_results/mh_varying_iterations.csv

# PF benchmarks with varying particles
echo "2. PF benchmarks with varying particles..."
dune exec bench_cli -- -alg pf -seqlens 10,20,50,100 -particles 25,50,100,200 -repeats 10 > benchmark_results/pf_varying_particles.csv

# Quick MH benchmark for testing
echo "3. Quick MH benchmark..."
dune exec bench_cli -- -alg mh -seqlens 10,20,50 -particles 100 -repeats 5 > benchmark_results/mh_quick.csv

# Quick PF benchmark for testing
echo "4. Quick PF benchmark..."
dune exec bench_cli -- -alg pf -seqlens 10,20,50 -particles 50,100 -repeats 5 > benchmark_results/pf_quick.csv

# Long sequence benchmarks
echo "5. Long sequence benchmarks..."
dune exec bench_cli -- -alg mh -seqlens 100,200,500 -particles 100,200 -repeats 10 > benchmark_results/mh_long_sequences.csv
dune exec bench_cli -- -alg pf -seqlens 100,200,500 -particles 50,100 -repeats 10 > benchmark_results/pf_long_sequences.csv

echo ""
echo "Benchmarks complete! Results saved in benchmark_results/"
echo ""
echo "Files created:"
ls -lh benchmark_results/
