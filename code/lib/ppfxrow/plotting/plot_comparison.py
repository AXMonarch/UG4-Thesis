#!/usr/bin/env python3
import csv
import matplotlib.pyplot as plt
import numpy as np

def read_csv_data(filename):
    """Read CSV and return as dict mapping row name to (x_values, y_values)."""
    data = {}
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        lines = list(reader)
        
    i = 0
    while i < len(lines):
        if not lines[i] or not lines[i][0].strip():
            i += 1
            continue
        
        # First line is the x-axis header
        x_label = lines[i][0]
        x_values = [float(x) for x in lines[i][1:] if x.strip()]
        
        # Subsequent lines are data rows until we hit another header or empty line
        i += 1
        while i < len(lines) and lines[i] and lines[i][0].strip() and not lines[i][0].startswith('Num '):
            row_name = lines[i][0]
            y_values = [float(y) for y in lines[i][1:] if y.strip()]
            data[row_name] = (x_values[:len(y_values)], y_values)
            i += 1
    
    return data

def plot_comparison(ocaml_data, haskell_data, experiment_name, x_label, y_label, output_file):
    """Create a comparison plot for OCaml vs Haskell."""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    ocaml_x, ocaml_y = ocaml_data
    haskell_x, haskell_y = haskell_data
    
    ax.plot(ocaml_x, ocaml_y, 'o-', linewidth=2, markersize=8, label='OCaml', color='#E69F00')
    ax.plot(haskell_x, haskell_y, 's-', linewidth=2, markersize=8, label='Haskell (prob-fx)', color='#56B4E9')
    
    ax.set_xlabel(x_label, fontsize=12, fontweight='bold')
    ax.set_ylabel(y_label, fontsize=12, fontweight='bold')
    ax.set_title(experiment_name, fontsize=14, fontweight='bold')
    ax.legend(fontsize=11, loc='best')
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved {output_file}")
    plt.close()

def plot_fused_experiment(ocaml_data, haskell_data, model_keys, model_names, 
                          experiment_title, x_label, y_label, output_file):
    """Create a fused plot with two subplots for both models."""
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle(experiment_title, fontsize=14, fontweight='bold')
    
    for idx, (model_key, model_name) in enumerate(zip(model_keys, model_names)):
        ax = axes[idx]
        
        if model_key in ocaml_data and model_key in haskell_data:
            ocaml_x, ocaml_y = ocaml_data[model_key]
            haskell_x, haskell_y = haskell_data[model_key]
            
            ax.plot(ocaml_x, ocaml_y, 'o-', linewidth=2, markersize=8, 
                   label='OCaml', color='#E69F00')
            ax.plot(haskell_x, haskell_y, 's-', linewidth=2, markersize=8, 
                   label='Haskell (prob-fx)', color='#56B4E9')
            
        ax.set_xlabel(x_label, fontsize=11, fontweight='bold')
        ax.set_ylabel(y_label, fontsize=11, fontweight='bold')
        ax.set_title(model_name, fontsize=12, fontweight='bold')
        ax.legend(fontsize=10, loc='best')
        ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved {output_file}")
    plt.close()

def main():
    # Read data
    ocaml_data = read_csv_data('ocaml_ssmh_benchmarks.csv')
    haskell_data = read_csv_data('benchmarks-prob-fx.csv')
    
    print("OCaml data keys:", list(ocaml_data.keys()))
    print("Haskell data keys:", list(haskell_data.keys()))
    
    # Experiment 1: Varying SSMH iterations (both LinReg and HMM with 50 observations/steps)
    plot_fused_experiment(
        ocaml_data,
        haskell_data,
        ['SSMH-[ ]-LinRegr-50', 'SSMH-[ ]-HidMark-50'],
        ['Linear Regression (50 observations)', 'Hidden Markov Model (50 steps)'],
        'Experiment 1: Varying SSMH Iterations',
        'Number of SSMH Steps',
        'Time (seconds)',
        'experiment1_varying_iterations.png'
    )
    
    # Experiment 2: Varying model size (both LinReg and HMM with 100 SSMH steps)
    plot_fused_experiment(
        ocaml_data,
        haskell_data,
        ['LinRegr-[ ]-SSMH-100', 'HidMark-[ ]-SSMH-100'],
        ['Linear Regression (100 SSMH steps)', 'Hidden Markov Model (100 SSMH steps)'],
        'Experiment 2: Varying Model Size',
        'Number of Observations/Time Steps',
        'Time (seconds)',
        'experiment2_varying_model_size.png'
    )
    
    print("\nAll comparison plots generated successfully!")

if __name__ == '__main__':
    main()
