#!/usr/bin/env python3
"""
Generate benchmark visualizations from CSV benchmark data.

Graph Type 1: Execution time vs particles/iterations (fixed observations)
Graph Type 2: Execution time vs observations (fixed particles/iterations)
"""

import argparse
import csv
import matplotlib.pyplot as plt
import sys
from pathlib import Path
from collections import defaultdict


def read_csv_data(csv_file):
    """Read CSV data and return headers and rows."""
    with open(csv_file, 'r') as f:
        lines = f.readlines()
    
    # Find the line with column headers (contains commas and looks like CSV headers)
    header_line_idx = None
    for i, line in enumerate(lines):
        # Look for a line that looks like CSV headers (contains expected column names)
        if 'Sequence_Length' in line or 'Num_Particles' in line:
            header_line_idx = i
            break
    
    if header_line_idx is None:
        raise ValueError("Could not find CSV header line in file")
    
    # Parse from the header line onwards
    csv_content = ''.join(lines[header_line_idx:])
    reader = csv.DictReader(csv_content.splitlines())
    headers = reader.fieldnames
    data = list(reader)
    
    return headers, data


def plot_type1_ssmh(data, output_path, model_name="Model"):
    """
    Type 1 for SSMH: Execution time vs Iterations (fixed sequence length).
    """
    # Group by sequence length
    grouped = defaultdict(lambda: {'iterations': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        iterations = int(row['Iterations'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        grouped[seq_len]['iterations'].append(iterations)
        grouped[seq_len]['times'].append(avg_time)
        grouped[seq_len]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for seq_len in sorted(grouped.keys()):
        iterations = grouped[seq_len]['iterations']
        times = grouped[seq_len]['times']
        std_devs = grouped[seq_len]['std_devs']
        
        # Sort by iterations
        sorted_data = sorted(zip(iterations, times, std_devs))
        iterations, times, std_devs = zip(*sorted_data)
        
        ax.plot(iterations, times, marker='o', 
                   label=f'Obs={seq_len}', linestyle='-')
    
    ax.set_xlabel('Number of Iterations', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'SSMH: Execution Time vs Iterations ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type1_mpf(data, output_path, model_name="Model"):
    """
    Type 1 for MPF: Execution time vs Particles (fixed sequence length).
    """
    # Group by sequence length
    grouped = defaultdict(lambda: {'particles': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        grouped[seq_len]['particles'].append(particles)
        grouped[seq_len]['times'].append(avg_time)
        grouped[seq_len]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for seq_len in sorted(grouped.keys()):
        particles = grouped[seq_len]['particles']
        times = grouped[seq_len]['times']
        std_devs = grouped[seq_len]['std_devs']
        
        # Sort by particles
        sorted_data = sorted(zip(particles, times, std_devs))
        particles, times, std_devs = zip(*sorted_data)
        
        ax.plot(particles, times, marker='o', 
                   label=f'Obs={seq_len}', linestyle='-')
    
    ax.set_xlabel('Number of Particles', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'MPF: Execution Time vs Particles ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type1_pmh(data, output_path, model_name="Model"):
    """
    Type 1 for PMH: Execution time vs Particles (fixed sequence length and iterations).
    PMH-50 indicates 50 MH updates that vary in the number of particles.
    """
    # Group by (sequence_length, iterations)
    grouped = defaultdict(lambda: {'particles': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        iterations = int(row['Iterations'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        key = (seq_len, iterations)
        grouped[key]['particles'].append(particles)
        grouped[key]['times'].append(avg_time)
        grouped[key]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for (seq_len, iterations) in sorted(grouped.keys()):
        particles = grouped[(seq_len, iterations)]['particles']
        times = grouped[(seq_len, iterations)]['times']
        std_devs = grouped[(seq_len, iterations)]['std_devs']
        
        # Sort by particles
        sorted_data = sorted(zip(particles, times, std_devs))
        particles, times, std_devs = zip(*sorted_data)
        
        ax.plot(particles, times, marker='o', 
                   label=f'PMH-{iterations} (Obs={seq_len})', linestyle='-')
    
    ax.set_xlabel('Number of Particles', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'PMH: Execution Time vs Particles ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type1_rmpf(data, output_path, model_name="Model"):
    """
    Type 1 for RMPF: Execution time vs MH moves (fixed sequence length and particles).
    RMPF-10 indicates 10 particles that vary in the number of MH updates.
    """
    # Group by (sequence_length, particles)
    grouped = defaultdict(lambda: {'moves': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        moves = int(row['Num_Moves'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        key = (seq_len, particles)
        grouped[key]['moves'].append(moves)
        grouped[key]['times'].append(avg_time)
        grouped[key]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for (seq_len, particles) in sorted(grouped.keys()):
        moves = grouped[(seq_len, particles)]['moves']
        times = grouped[(seq_len, particles)]['times']
        std_devs = grouped[(seq_len, particles)]['std_devs']
        
        # Sort by moves
        sorted_data = sorted(zip(moves, times, std_devs))
        moves, times, std_devs = zip(*sorted_data)
        
        ax.plot(moves, times, marker='o', 
                   label=f'RMPF-{particles} (Obs={seq_len})', linestyle='-')
    
    ax.set_xlabel('Number of MH Moves', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'RMPF: Execution Time vs MH Moves ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type2_ssmh(data, output_path, model_name="Model"):
    """
    Type 2 for SSMH: Execution time vs Observations (fixed iterations).
    """
    # Group by iterations
    grouped = defaultdict(lambda: {'seq_lens': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        iterations = int(row['Iterations'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        grouped[iterations]['seq_lens'].append(seq_len)
        grouped[iterations]['times'].append(avg_time)
        grouped[iterations]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for iterations in sorted(grouped.keys()):
        seq_lens = grouped[iterations]['seq_lens']
        times = grouped[iterations]['times']
        std_devs = grouped[iterations]['std_devs']
        
        # Sort by sequence length
        sorted_data = sorted(zip(seq_lens, times, std_devs))
        seq_lens, times, std_devs = zip(*sorted_data)
        
        ax.plot(seq_lens, times, marker='o', 
                   label=f'Iters={iterations}', linestyle='-')
    
    ax.set_xlabel('Number of Observations', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'SSMH: Execution Time vs Observations ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type2_mpf(data, output_path, model_name="Model"):
    """
    Type 2 for MPF: Execution time vs Observations (fixed particles).
    """
    # Group by particles
    grouped = defaultdict(lambda: {'seq_lens': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        grouped[particles]['seq_lens'].append(seq_len)
        grouped[particles]['times'].append(avg_time)
        grouped[particles]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for particles in sorted(grouped.keys()):
        seq_lens = grouped[particles]['seq_lens']
        times = grouped[particles]['times']
        std_devs = grouped[particles]['std_devs']
        
        # Sort by sequence length
        sorted_data = sorted(zip(seq_lens, times, std_devs))
        seq_lens, times, std_devs = zip(*sorted_data)
        
        ax.plot(seq_lens, times, marker='o', 
                   label=f'Particles={particles}', linestyle='-')
    
    ax.set_xlabel('Number of Observations', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'MPF: Execution Time vs Observations ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type2_pmh(data, output_path, model_name="Model"):
    """
    Type 2 for PMH: Execution time vs Observations (fixed particles and iterations).
    PMH-50-10 indicates 50 MH updates that use 10 particles.
    """
    # Group by (particles, iterations)
    grouped = defaultdict(lambda: {'seq_lens': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        iterations = int(row['Iterations'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        key = (iterations, particles)
        grouped[key]['seq_lens'].append(seq_len)
        grouped[key]['times'].append(avg_time)
        grouped[key]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for (iterations, particles) in sorted(grouped.keys()):
        seq_lens = grouped[(iterations, particles)]['seq_lens']
        times = grouped[(iterations, particles)]['times']
        std_devs = grouped[(iterations, particles)]['std_devs']
        
        # Sort by sequence length
        sorted_data = sorted(zip(seq_lens, times, std_devs))
        seq_lens, times, std_devs = zip(*sorted_data)
        
        ax.plot(seq_lens, times, marker='o', 
                   label=f'PMH-{iterations}-{particles}', linestyle='-')
    
    ax.set_xlabel('Number of Observations', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'PMH: Execution Time vs Observations ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def plot_type2_rmpf(data, output_path, model_name="Model"):
    """
    Type 2 for RMPF: Execution time vs Observations (fixed particles and moves).
    RMPF-10-1 indicates 10 particles that use 1 MH update.
    """
    # Group by (particles, moves)
    grouped = defaultdict(lambda: {'seq_lens': [], 'times': [], 'std_devs': []})
    
    for row in data:
        seq_len = int(row['Sequence_Length'])
        particles = int(row['Num_Particles'])
        moves = int(row['Num_Moves'])
        avg_time = float(row['Avg_Time_ms'])
        std_dev = float(row['Std_Dev_ms'])
        
        key = (particles, moves)
        grouped[key]['seq_lens'].append(seq_len)
        grouped[key]['times'].append(avg_time)
        grouped[key]['std_devs'].append(std_dev)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for (particles, moves) in sorted(grouped.keys()):
        seq_lens = grouped[(particles, moves)]['seq_lens']
        times = grouped[(particles, moves)]['times']
        std_devs = grouped[(particles, moves)]['std_devs']
        
        # Sort by sequence length
        sorted_data = sorted(zip(seq_lens, times, std_devs))
        seq_lens, times, std_devs = zip(*sorted_data)
        
        ax.plot(seq_lens, times, marker='o', 
                   label=f'RMPF-{particles}-{moves}', linestyle='-')
    
    ax.set_xlabel('Number of Observations', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontsize=12)
    ax.set_title(f'RMPF: Execution Time vs Observations ({model_name})', fontsize=14)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_path}")
    plt.close()


def detect_algorithm(headers):
    """Detect which algorithm based on CSV headers."""
    if 'Num_Moves' in headers:
        return 'rmpf'
    elif 'Num_Particles' in headers and 'Iterations' in headers:
        return 'pmh'
    elif 'Num_Particles' in headers:
        return 'mpf'
    elif 'Iterations' in headers:
        return 'ssmh'
    else:
        raise ValueError("Cannot detect algorithm from CSV headers")


def main():
    parser = argparse.ArgumentParser(
        description='Generate benchmark graphs from CSV data',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python plot_benchmarks.py -i ssmh_results.csv -o graphs/ -m "HMM"
  python plot_benchmarks.py -i pmh_results.csv -o graphs/ -t type1 -m "Linear Regression"
  python plot_benchmarks.py -i results.csv -o output/ -t both
        """
    )
    
    parser.add_argument('-i', '--input', required=True,
                       help='Input CSV file from benchmark')
    parser.add_argument('-o', '--output-dir', default='.',
                       help='Output directory for graphs (default: current directory)')
    parser.add_argument('-t', '--type', choices=['type1', 'type2', 'both'], 
                       default='both',
                       help='Type of graph to generate (default: both)')
    parser.add_argument('-m', '--model', default='Model',
                       help='Model name for graph titles (default: "Model")')
    parser.add_argument('-p', '--prefix', default='',
                       help='Prefix for output filenames')
    
    args = parser.parse_args()
    
    # Read CSV data
    try:
        headers, data = read_csv_data(args.input)
    except FileNotFoundError:
        print(f"Error: File '{args.input}' not found", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading CSV: {e}", file=sys.stderr)
        sys.exit(1)
    
    if not data:
        print("Error: CSV file contains no data", file=sys.stderr)
        sys.exit(1)
    
    # Detect algorithm
    algorithm = detect_algorithm(headers)
    print(f"Detected algorithm: {algorithm.upper()}")
    
    # Create output directory
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Generate filename prefix
    prefix = args.prefix if args.prefix else algorithm
    
    # Plot functions mapping
    plot_funcs = {
        'ssmh': {
            'type1': plot_type1_ssmh,
            'type2': plot_type2_ssmh
        },
        'mpf': {
            'type1': plot_type1_mpf,
            'type2': plot_type2_mpf
        },
        'pmh': {
            'type1': plot_type1_pmh,
            'type2': plot_type2_pmh
        },
        'rmpf': {
            'type1': plot_type1_rmpf,
            'type2': plot_type2_rmpf
        }
    }
    
    # Generate graphs
    if args.type in ['type1', 'both']:
        output_path = output_dir / f"{prefix}_type1.png"
        plot_funcs[algorithm]['type1'](data, output_path, args.model)
    
    if args.type in ['type2', 'both']:
        output_path = output_dir / f"{prefix}_type2.png"
        plot_funcs[algorithm]['type2'](data, output_path, args.model)
    
    print("Done!")


if __name__ == '__main__':
    main()
