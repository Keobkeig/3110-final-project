# OCaml Autodiff

An efficient and extensible automatic differentiation library for OCaml, supporting both reverse and forward mode AD.

## Team Members

- Richie Xue (rx77)
- Eric Chen (ec936)
- Jayden Lim (jcl399)
- Colby Bittner (cb954)

## Project Features (MS2 WIP)

- **Reverse-Mode AD:** Optimized for expressions with many parameters.
- **Forward-Mode AD:** Implemented via dual numbers for specific partial derivatives.
- **Expression Parsing:** Robust recursive-descent parser for arithmetic (`+`, `-`, `*`, `/`, `^`) and functions (`exp`, `ln`, `sin`, `cos`).
- **Gradient Checker:** Numerical verification suite to ensure AD accuracy.
- **Visualization:** Export expression computation graphs to Graphviz DOT format.
- **ML Training Engine:** Stochastic Gradient Descent (SGD) and Adam optimizer for linear models.

## Setup Instructions

Clone the repository:
```bash
git clone https://github.com/Keobkeig/ocaml-autodiff.git
cd ocaml-autodiff
```

Build the project:
```bash
dune build
```

## Usage

Use the CLI to differentiate expressions, train models, or export visualizations.

### 1. Differentiate (Reverse Mode)
```bash
dune exec bin/main.exe -- diff "x*x + 3*x + 1" "x=2"
```

### 2. Differentiate (Forward Mode)
```bash
dune exec bin/main.exe -- forward-diff "x*y + exp(x)" "x=1,y=2"
```

### 3. Run Training Demo (SGD)
```bash
dune exec bin/main.exe -- train 300 0.05
# Or with a small clean dataset (CSV with two columns: x,y):
dune exec bin/main.exe -- train 300 0.05 data/y_eq_2x_plus_1.csv
# Or with the larger noisy dataset (80 rows, true model y = 2.5x - 1.2 + noise):
dune exec bin/main.exe -- train 2000 0.01 data/y_eq_2.5x_minus_1.2_noisy.csv
```

### 4. Run Training Demo (Adam)
```bash
dune exec bin/main.exe -- train-adam 1000
# Or with a small clean dataset:
dune exec bin/main.exe -- train-adam 5000 data/y_eq_3x_minus_2.csv
# Or with the larger noisy dataset:
dune exec bin/main.exe -- train-adam 2000 data/y_eq_2.5x_minus_1.2_noisy.csv
```

Sample datasets are bundled under [data/](data/). The clean datasets
(`y_eq_2x_plus_1.csv`, `y_eq_3x_minus_2.csv`) are tiny and converge
instantly; the noisy dataset (`y_eq_2.5x_minus_1.2_noisy.csv`) is 80
points sampled around `y = 2.5x - 1.2` with Gaussian noise and is more
representative of a real fitting problem. CSV format is one `x,y` pair
per line; blank lines are ignored.

Note: Adam uses a fixed learning rate of `0.01`, while SGD's learning
rate is set on the command line. Adam's strength isn't raw speed on
simple problems — well-tuned SGD will match it — but rather its
adaptive per-parameter step size, which makes it robust on noisier
or higher-dimensional problems where picking a single SGD learning
rate is hard.

### 5. Export Visualization (requires 'graphviz')
```bash
dune exec bin/main.exe -- export-dot "x*x + sin(x)" expr.dot
dot -Tpng -Gdpi=600 expr.dot -o output.png
# We recommend dpi higher than 300 for best results.

# Saving as .svg: 
dot -Tsvg  expr.dot -o output.svg
```

### 6. Check Gradients
```bash
dune exec bin/main.exe -- check-grad "x*x" "x=2" x
```

## Testing & Coverage

Run the test suite:
```bash
dune test
```

Generate coverage report (requires `bisect_ppx`):
```bash
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
```

## MS2 Submission Files

- `INSTALL.md`: Detailed installation and environment setup.
- `AUTHORS.md`: Project contributors and citations.
- `gallery.yaml`: Project gallery metadata and demo link.
- `RepoURL.txt`: Official repository URL.
- `MS2Report.txt`: Progress report (to be converted to PDF).

