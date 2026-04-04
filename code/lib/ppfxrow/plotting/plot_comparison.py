#!/usr/bin/env python3
import csv
import matplotlib.pyplot as plt
import os

def read_csv_data(filename):
    data = {}
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        lines = list(reader)

    i = 0
    while i < len(lines):
        if not lines[i] or not lines[i][0].strip():
            i += 1
            continue

        x_label = lines[i][0]
        x_values = [float(x) for x in lines[i][1:] if x.strip()]

        i += 1
        while i < len(lines) and lines[i] and lines[i][0].strip() and not lines[i][0].startswith('Num '):
            row_name = lines[i][0]
            y_values = [float(y) for y in lines[i][1:] if y.strip()]
            data[row_name] = (x_values[:len(y_values)], y_values)
            i += 1

    return data


def plot_fused_experiment(ocaml_data, haskell_data, model_keys,
                          x_labels, y_label, output_file, titles=None):
    
    fig, axes = plt.subplots(1, 3, figsize=(18, 5))

    for idx, model_key in enumerate(model_keys):
        ax = axes[idx]

        if model_key in ocaml_data and model_key in haskell_data:
            ocaml_x, ocaml_y   = ocaml_data[model_key]
            haskell_x, haskell_y = haskell_data[model_key]

            ax.plot(ocaml_x,   ocaml_y,   'o-', linewidth=2, markersize=8,
                    label='OCaml',            color='#E69F00')
            ax.plot(haskell_x, haskell_y, 's-', linewidth=2, markersize=8,
                    label='Haskell (prob-fx)', color='#56B4E9')

        if titles and idx < len(titles):
            ax.set_title(titles[idx], fontsize=14, fontweight='bold')

        x_lbl = x_labels[idx] if isinstance(x_labels, list) else x_labels
        ax.set_xlabel(x_lbl, fontsize=14, fontweight='bold')

        if idx == 0:
            ax.set_ylabel(y_label, fontsize=14, fontweight='bold')
        else:
            ax.set_ylabel('')

        ax.tick_params(axis='both', labelsize=12)

        ax.legend(fontsize=13, loc='best')
        ax.grid(True, alpha=0.3)

    plt.tight_layout()

    filename     = os.path.basename(output_file)
    pictures_path = os.path.join('..','..', '..', '..', 'pictures', filename)
    plt.savefig(pictures_path, dpi=300, bbox_inches='tight')
    print(f"Saved {pictures_path}")
    plt.close()


def main():
    ocaml_data   = read_csv_data('ocaml_cap_benchmarks.csv')
    haskell_data = read_csv_data('benchmarks-prob-fx.csv')

    print("OCaml data keys:",   list(ocaml_data.keys()))
    print("Haskell data keys:", list(haskell_data.keys()))

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['SSMH-[ ]-LinRegr-50', 'SSMH-[ ]-HidMark-50', 'SSMH-[ ]-LatDiri-50'],
        x_labels=['Number of SSMH Steps'] * 3,
        y_label='Time (seconds)',
        output_file='plotting/ssmh_experiment1_varying_iterations.png',
        titles=['Linear Regression (50 obs)',
                'Hidden Markov Model (50 nodes)',
                'Latent Dirichlet Allocation (50 words)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['MPF-[ ]-LinRegr-50', 'MPF-[ ]-HidMark-50', 'MPF-[ ]-LatDiri-50'],
        x_labels=['Number of MPF Particles'] * 3,
        y_label='Time (seconds)',
        output_file='plotting/mpf_experiment1_varying_iterations.png',
        titles=['Linear Regression (50 obs)',
                'Hidden Markov Model (50 nodes)',
                'Latent Dirichlet Allocation (50 words)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['PMH-50-[ ]-LinRegr-50', 'PMH-50-[ ]-HidMark-50', 'PMH-50-[ ]-LatDiri-50'],
        x_labels=['Number of PMH Particles'] * 3,
        y_label='Time (seconds)',
        output_file='plotting/pmh_experiment1_varying_iterations.png',
        titles=['Linear Regression (50 obs)',
                'Hidden Markov Model (50 nodes)',
                'Latent Dirichlet Allocation (50 words)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['RMPF-10-[ ]-LinRegr-50', 'RMPF-10-[ ]-HidMark-50', 'RMPF-10-[ ]-LatDiri-50'],
        x_labels=['Number of RMPF MH Steps'] * 3,
        y_label='Time (seconds)',
        output_file='plotting/rmpf_experiment1_varying_iterations.png',
        titles=['Linear Regression (50 obs)',
                'Hidden Markov Model (50 nodes)',
                'Latent Dirichlet Allocation (50 words)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['LinRegr-[ ]-SSMH-100', 'HidMark-[ ]-SSMH-100', 'LatDiri-[ ]-SSMH-100'],
        x_labels=['Number of Datapoints',
                  'Number of Nodes',
                  'Number of Words'],
        y_label='Time (seconds)',
        output_file='plotting/ssmh_experiment2_varying_model_size.png',
        titles=['Linear Regression (100 steps)',
                'Hidden Markov Model (100 steps)',
                'Latent Dirichlet Allocation (100 steps)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['LinRegr-[ ]-MPF-100', 'HidMark-[ ]-MPF-100', 'LatDiri-[ ]-MPF-100'],
        x_labels=['Number of Datapoints',
                  'Number of Nodes',
                  'Number of Words'],
        y_label='Time (seconds)',
        output_file='plotting/mpf_experiment2_varying_model_size.png',
        titles=['Linear Regression (100 particles)',
                'Hidden Markov Model (100 particles)',
                'Latent Dirichlet Allocation (100 particles)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['LinRegr-[ ]-PMH-50-10', 'HidMark-[ ]-PMH-50-10', 'LatDiri-[ ]-PMH-50-10'],
        x_labels=['Number of Datapoints',
                  'Number of Nodes',
                  'Number of Words'],
        y_label='Time (seconds)',
        output_file='plotting/pmh_experiment2_varying_model_size.png',
        titles=['Linear Regression (50-10)',
                'Hidden Markov Model (50-10)',
                'Latent Dirichlet Allocation (50-10)']
    )

    plot_fused_experiment(
        ocaml_data, haskell_data,
        ['LinRegr-[ ]-RMPF-10-1', 'HidMark-[ ]-RMPF-10-1', 'LatDiri-[ ]-RMPF-10-1'],
        x_labels=['Number of Datapoints',
                  'Number of Nodes',
                  'Number of Words'],
        y_label='Time (seconds)',
        output_file='plotting/rmpf_experiment2_varying_model_size.png',
        titles=['Linear Regression (10-1)',
                'Hidden Markov Model (10-1)',
                'Latent Dirichlet Allocation (10-1)']
    )

    print("\nAll comparison plots generated successfully!")


if __name__ == '__main__':
    main()