#!/usr/bin/env python3
"""
Plot OCaml vs ProbFX benchmark comparisons.

This script creates comparison plots for:
1. Model size experiments (varying datapoints/nodes with fixed algorithm)
2. Algorithm parameter experiments (varying algorithm params with fixed model size)
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path

# Configuration
PROBFX_FILE = "../../benchmarks-prob-fx.csv"
OCAML_FILE = "../benchmarks_ocaml_fused.csv"
OUTPUT_DIR = "../graphs/comparison_plots"

def read_fused_csv(filepath):
    """
    Read the fused CSV format where each row represents an experiment configuration.
    Returns a dictionary mapping experiment names to their data.
    """
    experiments = {}
    current_header = None
    
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            
            parts = line.split(',')
            
            # Check if this is a header row (starts with "Num")
            if parts[0].startswith('Num'):
                current_header = parts[0]
                x_values = [float(x) for x in parts[1:]]
                continue
            
            # This is a data row
            experiment_name = parts[0]
            y_values = [float(y) * 1000.0 for y in parts[1:]]  # Convert seconds to milliseconds
            
            experiments[experiment_name] = {
                'x_label': current_header,
                'x_values': x_values,
                'y_values': y_values
            }
    
    return experiments

def parse_experiment_name(name):
    """
    Parse experiment name to extract model, algorithm, and parameters.
    Example: "LinRegr-[ ]-SSMH-100" -> model="LinRegr", algo="SSMH", params="100"
    """
    parts = name.split('-[ ]-')
    if len(parts) != 2:
        return None, None, None
    
    model = parts[0]
    algo_parts = parts[1].split('-')
    algo = algo_parts[0]
    params = '-'.join(algo_parts[1:]) if len(algo_parts) > 1 else ''
    
    return model, algo, params

def create_comparison_plot(ocaml_exp, probfx_exp, title, output_file):
    """
    Create a single comparison plot for OCaml vs ProbFX.
    """
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Plot OCaml
    ax.plot(ocaml_exp['x_values'], ocaml_exp['y_values'], 
            marker='o', linewidth=2, markersize=8, 
            label='OCaml (Effects)', color='#2E86AB')
    
    # Plot ProbFX
    ax.plot(probfx_exp['x_values'], probfx_exp['y_values'], 
            marker='s', linewidth=2, markersize=8, 
            label='ProbFX (Haskell)', color='#A23B72')
    
    ax.set_xlabel(ocaml_exp['x_label'], fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(title, fontsize=14, fontweight='bold', pad=20)
    ax.legend(fontsize=11, frameon=True, shadow=True)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    plt.close()
    
    print(f"Saved: {output_file}")

def create_all_comparisons():
    """
    Skip individual comparison plots - only create summaries.
    """
    # Read both CSV files
    print("Reading OCaml benchmarks...")
    ocaml_exps = read_fused_csv(OCAML_FILE)
    
    print("Reading ProbFX benchmarks...")
    probfx_exps = read_fused_csv(PROBFX_FILE)
    
    print(f"\nOCaml experiments found: {len(ocaml_exps)}")
    print(f"ProbFX experiments found: {len(probfx_exps)}")
    
    # Return the data for other functions to use
    return ocaml_exps, probfx_exps

def create_summary_plots(ocaml_exps, probfx_exps):
    """
    Create summary plots grouping by algorithm type.
    """
    print("\nCreating summary plots...")
    
    # Group by algorithm
    algorithms = ['SSMH', 'MPF', 'PMH', 'RMPF']
    models = ['LinRegr', 'HidMark']
    
    for algo in algorithms:
        fig, axes = plt.subplots(1, 2, figsize=(16, 6))
        fig.suptitle(f'{algo} Algorithm Comparison: OCaml vs ProbFX', 
                     fontsize=16, fontweight='bold')
        
        for idx, model in enumerate(models):
            ax = axes[idx]
            
            # Find matching experiments
            matching_exps = [name for name in ocaml_exps.keys() 
                           if model in name and f'-{algo}-' in name]
            
            if not matching_exps:
                continue
            
            # Plot first matching experiment (varying model size)
            exp_name = matching_exps[0]
            
            if exp_name in ocaml_exps and exp_name in probfx_exps:
                ocaml_data = ocaml_exps[exp_name]
                probfx_data = probfx_exps[exp_name]
                
                ax.plot(ocaml_data['x_values'], ocaml_data['y_values'],
                       marker='o', linewidth=2, markersize=8,
                       label='OCaml (Effects)', color='#2E86AB')
                
                ax.plot(probfx_data['x_values'], probfx_data['y_values'],
                       marker='s', linewidth=2, markersize=8,
                       label='ProbFX (Haskell)', color='#A23B72')
                
                ax.set_xlabel(ocaml_data['x_label'], fontsize=11, fontweight='bold')
                ax.set_ylabel('Execution Time (ms)', fontsize=11, fontweight='bold')
                ax.set_title(f'{model} Model', fontsize=12, fontweight='bold')
                ax.legend(fontsize=10, frameon=True, shadow=True)
                ax.grid(True, alpha=0.3, linestyle='--')
        
        plt.tight_layout()
        output_file = f"{OUTPUT_DIR}/summary_{algo}_comparison.png"
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved summary: {output_file}")

def generate_speedup_table():
    """
    Generate a table showing speedup factors (OCaml vs ProbFX).
    """
    ocaml_exps = read_fused_csv(OCAML_FILE)
    probfx_exps = read_fused_csv(PROBFX_FILE)
    
    print("\n" + "="*80)
    print("SPEEDUP ANALYSIS (ProbFX Time / OCaml Time)")
    print("="*80)
    print(f"{'Experiment':<40} {'Avg Speedup':<15} {'Min':<10} {'Max':<10}")
    print("-"*80)
    
    for exp_name in sorted(ocaml_exps.keys()):
        if exp_name in probfx_exps:
            ocaml_times = np.array(ocaml_exps[exp_name]['y_values'])
            probfx_times = np.array(probfx_exps[exp_name]['y_values'])
            
            # Calculate speedup (>1 means OCaml is faster)
            speedups = probfx_times / ocaml_times
            
            avg_speedup = np.mean(speedups)
            min_speedup = np.min(speedups)
            max_speedup = np.max(speedups)
            
            # Interpret speedup
            interpretation = "OCaml faster" if avg_speedup > 1 else "ProbFX faster"
            
            print(f"{exp_name:<40} {avg_speedup:>6.2f}x {interpretation:<20} "
                  f"{min_speedup:>6.2f}x   {max_speedup:>6.2f}x")
    
    print("="*80 + "\n")

if __name__ == "__main__":
    print("OCaml vs ProbFX Benchmark Comparison Tool")
    print("="*60)
    
    try:
        # Read comparison data
        ocaml_exps, probfx_exps = create_all_comparisons()
        
        # Create summary plots only
        create_summary_plots(ocaml_exps, probfx_exps)
        
        # Generate speedup analysis
        generate_speedup_table()
        
        print("\n✓ Summary plots generated successfully!")
        print(f"✓ Check the graphs folder for summary_*.png files")
        
    except FileNotFoundError as e:
        print(f"\n✗ Error: {e}")
        print("\nPlease ensure:")
        print(f"  1. ProbFX benchmarks exist at: {PROBFX_FILE}")
        print(f"  2. OCaml benchmarks exist at: {OCAML_FILE}")
        print("\nRun the OCaml benchmarks first if they don't exist.")
    except Exception as e:
        print(f"\n✗ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
