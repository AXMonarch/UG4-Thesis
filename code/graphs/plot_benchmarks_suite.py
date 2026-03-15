#!/usr/bin/env python3
"""
Comprehensive Benchmark Visualization Suite

Generates publication-quality graphs from benchmark CSV data for:
1. Varying Dataset Size experiments
2. Varying Algorithm Parameters experiments

Usage:
    python3 plot_benchmarks_suite.py <results_directory>
    
Example:
    python3 plot_benchmarks_suite.py benchmark_results/
"""

import argparse
import csv
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
from collections import defaultdict
import sys


def read_csv_data(csv_file):
    """Read CSV data and return headers and rows."""
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        headers = reader.fieldnames
        data = list(reader)
    return headers, data


def setup_plot_style():
    """Configure matplotlib style for publication-quality plots."""
    plt.rcParams['font.size'] = 11
    plt.rcParams['axes.labelsize'] = 12
    plt.rcParams['axes.titlesize'] = 14
    plt.rcParams['xtick.labelsize'] = 10
    plt.rcParams['ytick.labelsize'] = 10
    plt.rcParams['legend.fontsize'] = 10
    plt.rcParams['figure.titlesize'] = 16


def plot_varying_dataset_comparison(data_dict, output_dir, model_name):
    """
    Plot comparison of all algorithms with varying dataset sizes.
    
    Args:
        data_dict: Dictionary with algorithm names as keys and data lists as values
        output_dir: Directory to save plots
        model_name: Name of the model (e.g., 'LinearRegression', 'HMM')
    """
    fig, ax = plt.subplots(figsize=(10, 6))
    
    markers = {'SSMH': 'o', 'MPF': 's', 'PMH': '^', 'RMPF': 'D'}
    colors = {'SSMH': '#1f77b4', 'MPF': '#ff7f0e', 'PMH': '#2ca02c', 'RMPF': '#d62728'}
    
    for algo_name, data in sorted(data_dict.items()):
        dataset_sizes = []
        avg_times = []
        std_devs = []
        
        for row in data:
            dataset_sizes.append(int(row['Dataset_Size']))
            avg_times.append(float(row['Avg_Time_ms']))
            std_devs.append(float(row['Std_Dev_ms']))
        
        # Sort by dataset size
        sorted_data = sorted(zip(dataset_sizes, avg_times, std_devs))
        dataset_sizes, avg_times, std_devs = zip(*sorted_data)
        
        ax.errorbar(dataset_sizes, avg_times, yerr=std_devs,
                   marker=markers.get(algo_name, 'o'),
                   color=colors.get(algo_name, None),
                   label=algo_name, linewidth=2, markersize=6,
                   capsize=3, capthick=1.5, alpha=0.8)
    
    ax.set_xlabel('Dataset Size', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: Scalability Analysis\nExecution Time vs Dataset Size',
                fontsize=14, fontweight='bold')
    ax.legend(loc='best', framealpha=0.9)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    plt.tight_layout()
    output_path = output_dir / f'varying_dataset_{model_name.lower()}_comparison.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def plot_varying_params_ssmh(data, output_dir, model_name):
    """Plot SSMH iterations vs execution time."""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    iterations = []
    avg_times = []
    std_devs = []
    
    for row in data:
        iterations.append(int(row['Iterations']))
        avg_times.append(float(row['Avg_Time_ms']))
        std_devs.append(float(row['Std_Dev_ms']))
    
    sorted_data = sorted(zip(iterations, avg_times, std_devs))
    iterations, avg_times, std_devs = zip(*sorted_data)
    
    ax.errorbar(iterations, avg_times, yerr=std_devs,
               marker='o', color='#1f77b4', linewidth=2, markersize=7,
               capsize=4, capthick=2, alpha=0.8, label='SSMH')
    
    ax.set_xlabel('Number of Iterations', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: SSMH Performance\nExecution Time vs Iterations',
                fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3, linestyle='--')
    ax.legend()
    
    plt.tight_layout()
    output_path = output_dir / f'varying_params_ssmh_{model_name.lower()}.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def plot_varying_params_mpf(data, output_dir, model_name):
    """Plot MPF particles vs execution time."""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    particles = []
    avg_times = []
    std_devs = []
    
    for row in data:
        particles.append(int(row['Num_Particles']))
        avg_times.append(float(row['Avg_Time_ms']))
        std_devs.append(float(row['Std_Dev_ms']))
    
    sorted_data = sorted(zip(particles, avg_times, std_devs))
    particles, avg_times, std_devs = zip(*sorted_data)
    
    ax.errorbar(particles, avg_times, yerr=std_devs,
               marker='s', color='#ff7f0e', linewidth=2, markersize=7,
               capsize=4, capthick=2, alpha=0.8, label='MPF')
    
    ax.set_xlabel('Number of Particles', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: MPF Performance\nExecution Time vs Particles',
                fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3, linestyle='--')
    ax.legend()
    
    plt.tight_layout()
    output_path = output_dir / f'varying_params_mpf_{model_name.lower()}.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def plot_varying_params_pmh(data, output_dir, model_name):
    """Plot PMH particles vs execution time (fixed MH steps)."""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    particles = []
    avg_times = []
    std_devs = []
    
    for row in data:
        particles.append(int(row['Num_Particles']))
        avg_times.append(float(row['Avg_Time_ms']))
        std_devs.append(float(row['Std_Dev_ms']))
    
    sorted_data = sorted(zip(particles, avg_times, std_devs))
    particles, avg_times, std_devs = zip(*sorted_data)
    
    ax.errorbar(particles, avg_times, yerr=std_devs,
               marker='^', color='#2ca02c', linewidth=2, markersize=7,
               capsize=4, capthick=2, alpha=0.8, label='PMH')
    
    ax.set_xlabel('Number of Particles', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: PMH Performance\nExecution Time vs Particles (50 MH Steps)',
                fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3, linestyle='--')
    ax.legend()
    
    plt.tight_layout()
    output_path = output_dir / f'varying_params_pmh_{model_name.lower()}.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def plot_varying_params_rmpf(data, output_dir, model_name):
    """Plot RMPF MH steps vs execution time (fixed particles)."""
    fig, ax = plt.subplots(figsize=(10, 6))
    
    mh_steps = []
    avg_times = []
    std_devs = []
    
    for row in data:
        mh_steps.append(int(row['MH_Steps']))
        avg_times.append(float(row['Avg_Time_ms']))
        std_devs.append(float(row['Std_Dev_ms']))
    
    sorted_data = sorted(zip(mh_steps, avg_times, std_devs))
    mh_steps, avg_times, std_devs = zip(*sorted_data)
    
    ax.errorbar(mh_steps, avg_times, yerr=std_devs,
               marker='D', color='#d62728', linewidth=2, markersize=7,
               capsize=4, capthick=2, alpha=0.8, label='RMPF')
    
    ax.set_xlabel('Number of MH Steps', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: RMPF Performance\nExecution Time vs MH Steps (10 Particles)',
                fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3, linestyle='--')
    ax.legend()
    
    plt.tight_layout()
    output_path = output_dir / f'varying_params_rmpf_{model_name.lower()}.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def plot_all_varying_params_comparison(data_dict, output_dir, model_name):
    """
    Plot comparison of all parameter variations on a single graph.
    Normalizes different x-axes using percentile scale.
    """
    fig, ax = plt.subplots(figsize=(12, 7))
    
    markers = {'SSMH': 'o', 'MPF': 's', 'PMH': '^', 'RMPF': 'D'}
    colors = {'SSMH': '#1f77b4', 'MPF': '#ff7f0e', 'PMH': '#2ca02c', 'RMPF': '#d62728'}
    
    for algo_name, data in sorted(data_dict.items()):
        x_values = []
        y_values = []
        errors = []
        
        for row in data:
            y_values.append(float(row['Avg_Time_ms']))
            errors.append(float(row['Std_Dev_ms']))
            
            # Determine x-axis value based on algorithm
            if algo_name == 'SSMH':
                x_values.append(int(row['Iterations']))
            elif algo_name in ['MPF', 'PMH']:
                x_values.append(int(row['Num_Particles']))
            elif algo_name == 'RMPF':
                x_values.append(int(row['MH_Steps']))
        
        sorted_data = sorted(zip(x_values, y_values, errors))
        x_values, y_values, errors = zip(*sorted_data)
        
        # Normalize x to percentile [0, 100]
        x_normalized = np.linspace(0, 100, len(x_values))
        
        ax.errorbar(x_normalized, y_values, yerr=errors,
                   marker=markers.get(algo_name, 'o'),
                   color=colors.get(algo_name, None),
                   label=algo_name, linewidth=2, markersize=6,
                   capsize=3, capthick=1.5, alpha=0.8)
    
    ax.set_xlabel('Parameter Value (Normalized %)', fontsize=12, fontweight='bold')
    ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
    ax.set_title(f'{model_name}: Algorithm Parameter Sensitivity\nExecution Time vs Parameter Scaling',
                fontsize=14, fontweight='bold')
    ax.legend(loc='best', framealpha=0.9)
    ax.grid(True, alpha=0.3, linestyle='--')
    
    plt.tight_layout()
    output_path = output_dir / f'varying_params_{model_name.lower()}_all_comparison.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"✓ Saved: {output_path}")
    plt.close()


def process_benchmark_suite(results_dir):
    """Process all benchmark results and generate visualizations."""
    results_path = Path(results_dir)
    output_dir = results_path / 'graphs'
    output_dir.mkdir(exist_ok=True)
    
    setup_plot_style()
    
    print("\n=== Processing Varying Dataset Benchmarks ===\n")
    
    # Process varying dataset benchmarks for each model
    for model in ['linregr', 'hmm']:
        model_name = 'LinearRegression' if model == 'linregr' else 'HMM'
        print(f"\n{model_name}:")
        
        data_dict = {}
        for algo in ['ssmh', 'mpf', 'pmh', 'rmpf']:
            csv_file = results_path / f'varying_dataset_{algo}_{model}.csv'
            if csv_file.exists():
                _, data = read_csv_data(csv_file)
                data_dict[algo.upper()] = data
        
        if data_dict:
            plot_varying_dataset_comparison(data_dict, output_dir, model_name)
    
    print("\n\n=== Processing Varying Parameters Benchmarks ===\n")
    
    # Process varying parameters benchmarks for each model
    for model in ['linregr', 'hmm']:
        model_name = 'LinearRegression' if model == 'linregr' else 'HMM'
        print(f"\n{model_name}:")
        
        data_dict = {}
        
        # SSMH
        csv_file = results_path / f'varying_params_ssmh_{model}.csv'
        if csv_file.exists():
            _, data = read_csv_data(csv_file)
            data_dict['SSMH'] = data
            plot_varying_params_ssmh(data, output_dir, model_name)
        
        # MPF
        csv_file = results_path / f'varying_params_mpf_{model}.csv'
        if csv_file.exists():
            _, data = read_csv_data(csv_file)
            data_dict['MPF'] = data
            plot_varying_params_mpf(data, output_dir, model_name)
        
        # PMH
        csv_file = results_path / f'varying_params_pmh_{model}.csv'
        if csv_file.exists():
            _, data = read_csv_data(csv_file)
            data_dict['PMH'] = data
            plot_varying_params_pmh(data, output_dir, model_name)
        
        # RMPF
        csv_file = results_path / f'varying_params_rmpf_{model}.csv'
        if csv_file.exists():
            _, data = read_csv_data(csv_file)
            data_dict['RMPF'] = data
            plot_varying_params_rmpf(data, output_dir, model_name)
        
        # Combined comparison
        if data_dict:
            plot_all_varying_params_comparison(data_dict, output_dir, model_name)
    
    print(f"\n\n=== All Graphs Generated ===")
    print(f"Output directory: {output_dir}/")
    print(f"\nGenerated files:")
    for graph_file in sorted(output_dir.glob('*.png')):
        print(f"  - {graph_file.name}")


def main():
    parser = argparse.ArgumentParser(
        description='Generate benchmark visualizations from CSV data',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('results_dir', 
                       help='Directory containing benchmark CSV files')
    
    args = parser.parse_args()
    
    results_path = Path(args.results_dir)
    if not results_path.exists():
        print(f"Error: Directory '{args.results_dir}' does not exist")
        sys.exit(1)
    
    csv_files = list(results_path.glob('*.csv'))
    if not csv_files:
        print(f"Error: No CSV files found in '{args.results_dir}'")
        sys.exit(1)
    
    print(f"Found {len(csv_files)} CSV files in {args.results_dir}")
    process_benchmark_suite(args.results_dir)
    print("\n✓ Benchmark visualization complete!\n")


if __name__ == '__main__':
    main()
