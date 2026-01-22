#!/bin/bash

# OCaml vs ProbFX Benchmark Comparison Script
# This script runs OCaml benchmarks and generates comparison plots

set -e  # Exit on error

echo "=========================================="
echo "OCaml vs ProbFX Benchmark Comparison"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

cd "$(dirname "$0")/.."

# Step 1: Build the OCaml benchmark runner
echo -e "${BLUE}Step 1: Building OCaml benchmark runner...${NC}"
dune build run_all_benchmarks.exe
echo -e "${GREEN}✓ Build complete${NC}"
echo ""

# Step 2: Run OCaml benchmarks
echo -e "${BLUE}Step 2: Running OCaml benchmarks...${NC}"
echo "This may take several minutes depending on your machine."
echo "Progress will be shown as benchmarks complete."
echo ""

time ./_build/default/run_all_benchmarks.exe

echo ""
echo -e "${GREEN}✓ OCaml benchmarks complete${NC}"
echo ""

# Step 3: Check if Python and required packages are available
echo -e "${BLUE}Step 3: Checking Python environment...${NC}"

if ! command -v python3 &> /dev/null; then
    echo -e "${YELLOW}Warning: python3 not found. Please install Python 3 to generate plots.${NC}"
    exit 1
fi

# Check for required Python packages
python3 -c "import pandas, matplotlib, numpy" 2>/dev/null
if [ $? -ne 0 ]; then
    echo -e "${YELLOW}Installing required Python packages...${NC}"
    pip3 install pandas matplotlib numpy
fi

echo -e "${GREEN}✓ Python environment ready${NC}"
echo ""

# Step 4: Generate comparison plots
echo -e "${BLUE}Step 4: Generating comparison plots...${NC}"
cd benchmarks
python3 plot_ocaml_vs_probfx.py

echo ""
echo -e "${GREEN}=========================================="
echo "✓ All tasks complete!"
echo "==========================================${NC}"
echo ""
echo "Results:"
echo "  - OCaml benchmarks: benchmarks_ocaml_fused.csv"
echo "  - Comparison plots: graphs/comparison_plots/"
echo "  - Run script location: benchmarks/run_comparison.sh"
echo ""
echo "You can now:"
echo "  1. View the comparison plots in graphs/comparison_plots/"
echo "  2. Check the speedup analysis printed above"
echo "  3. Use the CSV files for further analysis"
echo ""
