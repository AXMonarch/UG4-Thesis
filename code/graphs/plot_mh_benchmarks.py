#!/usr/bin/env python3
"""
Generate benchmark visualizations for MH variants.

Creates two types of graphs:
1. Execution Time vs Dataset Size (Experiment 1)
2. Execution Time vs Iterations (Experiment 2)

For both Single-Site MH and Particle MH on HMM and Linear Regression models.
"""

import csv
import matplotlib.pyplot as plt
from pathlib import Path


def read_csv_data(csv_file):
    """Read CSV data and return data rows."""
    data = []
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            data.append(row)
    return data


def plot_experiment1(output_dir="graphs"):
    """
    Experiment 1: Execution Time vs Dataset Size
    Fixed iterations = 100
    """
    variants = ['singlesitemh', 'particlemh']
    models = ['hmm', 'linregr']
    variant_names = {'singlesitemh': 'Single-Site MH', 'particlemh': 'Particle MH'}
    model_names = {'hmm': 'HMM', 'linregr': 'Linear Regression'}
    
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle('Experiment 1: Execution Time vs Dataset Size (Fixed 100 Iterations)', 
                 fontsize=14, fontweight='bold')
    
    for model_idx, model in enumerate(models):
        ax = axes[model_idx]
        
        for variant in variants:
            csv_file = f"benchmark_results/varying_dataset_{variant}_{model}.csv"
            
            try:
                data = read_csv_data(csv_file)
                sizes = [int(row['Dataset_Size']) for row in data]
                times = [float(row['Avg_Time_ms']) for row in data]
                
                ax.plot(sizes, times, marker='o', linewidth=2, 
                       label=variant_names[variant], markersize=6)
            except FileNotFoundError:
                print(f"Warning: {csv_file} not found, skipping...")
        
        ax.set_xlabel('Dataset Size', fontsize=11)
        ax.set_ylabel('Execution Time (ms)', fontsize=11)
        ax.set_title(model_names[model], fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=10)
    
    plt.tight_layout()
    output_path = Path(output_dir) / "experiment1_dataset_size.png"
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_experiment2(output_dir="graphs"):
    """
    Experiment 2: Execution Time vs Iterations
    Fixed dataset size = 50
    """
    variants = ['singlesitemh', 'particlemh']
    models = ['hmm', 'linregr']
    variant_names = {'singlesitemh': 'Single-Site MH', 'particlemh': 'Particle MH'}
    model_names = {'hmm': 'HMM', 'linregr': 'Linear Regression'}
    
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    fig.suptitle('Experiment 2: Execution Time vs Iterations (Fixed Dataset Size 50)', 
                 fontsize=14, fontweight='bold')
    
    for model_idx, model in enumerate(models):
        ax = axes[model_idx]
        
        for variant in variants:
            csv_file = f"benchmark_results/varying_iterations_{variant}_{model}.csv"
            
            try:
                data = read_csv_data(csv_file)
                iterations = [int(row['Num_Iterations']) for row in data]
                times = [float(row['Avg_Time_ms']) for row in data]
                
                ax.plot(iterations, times, marker='o', linewidth=2, 
                       label=variant_names[variant], markersize=6)
            except FileNotFoundError:
                print(f"Warning: {csv_file} not found, skipping...")
        
        ax.set_xlabel('Number of Iterations', fontsize=11)
        ax.set_ylabel('Execution Time (ms)', fontsize=11)
        ax.set_title(model_names[model], fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=10)
    
    plt.tight_layout()
    output_path = Path(output_dir) / "experiment2_iterations.png"
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_combined_comparison(output_dir="graphs"):
    """
    Create a 2x2 grid showing both experiments for both models
    """
    variants = ['singlesitemh', 'particlemh']
    variant_names = {'singlesitemh': 'Single-Site MH', 'particlemh': 'Particle MH'}
    
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle('MH Variants Performance Comparison', fontsize=16, fontweight='bold')
    
    # Experiment 1 - HMM
    ax = axes[0, 0]
    for variant in variants:
        csv_file = f"benchmark_results/varying_dataset_{variant}_hmm.csv"
        try:
            data = read_csv_data(csv_file)
            sizes = [int(row['Dataset_Size']) for row in data]
            times = [float(row['Avg_Time_ms']) for row in data]
            ax.plot(sizes, times, marker='o', linewidth=2, label=variant_names[variant], markersize=6)
        except FileNotFoundError:
            pass
    ax.set_xlabel('Dataset Size', fontsize=11)
    ax.set_ylabel('Execution Time (ms)', fontsize=11)
    ax.set_title('Experiment 1: HMM (Fixed 100 Iterations)', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    
    # Experiment 1 - LinRegr
    ax = axes[0, 1]
    for variant in variants:
        csv_file = f"benchmark_results/varying_dataset_{variant}_linregr.csv"
        try:
            data = read_csv_data(csv_file)
            sizes = [int(row['Dataset_Size']) for row in data]
            times = [float(row['Avg_Time_ms']) for row in data]
            ax.plot(sizes, times, marker='o', linewidth=2, label=variant_names[variant], markersize=6)
        except FileNotFoundError:
            pass
    ax.set_xlabel('Dataset Size', fontsize=11)
    ax.set_ylabel('Execution Time (ms)', fontsize=11)
    ax.set_title('Experiment 1: Linear Regression (Fixed 100 Iterations)', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    
    # Experiment 2 - HMM
    ax = axes[1, 0]
    for variant in variants:
        csv_file = f"benchmark_results/varying_iterations_{variant}_hmm.csv"
        try:
            data = read_csv_data(csv_file)
            iterations = [int(row['Num_Iterations']) for row in data]
            times = [float(row['Avg_Time_ms']) for row in data]
            ax.plot(iterations, times, marker='o', linewidth=2, label=variant_names[variant], markersize=6)
        except FileNotFoundError:
            pass
    ax.set_xlabel('Number of Iterations', fontsize=11)
    ax.set_ylabel('Execution Time (ms)', fontsize=11)
    ax.set_title('Experiment 2: HMM (Fixed Dataset Size 50)', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    
    # Experiment 2 - LinRegr
    ax = axes[1, 1]
    for variant in variants:
        csv_file = f"benchmark_results/varying_iterations_{variant}_linregr.csv"
        try:
            data = read_csv_data(csv_file)
            iterations = [int(row['Num_Iterations']) for row in data]
            times = [float(row['Avg_Time_ms']) for row in data]
            ax.plot(iterations, times, marker='o', linewidth=2, label=variant_names[variant], markersize=6)
        except FileNotFoundError:
            pass
    ax.set_xlabel('Number of Iterations', fontsize=11)
    ax.set_ylabel('Execution Time (ms)', fontsize=11)
    ax.set_title('Experiment 2: Linear Regression (Fixed Dataset Size 50)', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    
    plt.tight_layout()
    output_path = Path(output_dir) / "mh_variants_combined.png"
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def main():
    """Generate all MH benchmark plots."""
    print("Generating MH benchmark plots...")
    
    # Create graphs directory if it doesn't exist
    Path("graphs").mkdir(exist_ok=True)
    
    # Generate individual experiment plots
    plot_experiment1()
    plot_experiment2()
    
    # Generate combined comparison plot
    plot_combined_comparison()
    
    print("\nAll plots generated successfully!")


if __name__ == "__main__":
    main()
