# Replication and extension of Meyersson (2014)

TECH6 Data Science and Econometrics, NHH, autumn 2026. Candidates 19, 34 and 42.

## Overview

This repository replicates Tables 1, 2, 3 and 7 and Figures 1 to 4 of Meyersson (2014) and
adds two extensions. Two master files run everything: `main.py` (Python) makes the
predictions used in Extension 2, and `run_all.R` (R) then produces every table, figure and
extension result from the original data.

## Structure

```text
TECH6-Collaborative-Project-19-34-42/
├── README.md
├── TECH6 Collaborative Project, 19, 34, 42.Rproj
├── main.py
├── run_all.R
├── data/
│   └── regdata0.dta
├── code/
│   ├── 00_setup.R
│   ├── 01_table1.R
│   ├── 02_table2.R
│   ├── 03_table3.R
│   ├── 04_table7.R
│   ├── 05_figure1.R
│   ├── 06_figure2.R
│   ├── 07_figure3.R
│   ├── 08_figure4.R
│   ├── extension1/
│   │   ├── 01_sample.R
│   │   ├── 02_main_results.R
│   │   ├── 03_control_vs_difference.R
│   │   └── 04_robustness.R
│   └── extension2/
│       └── 01_rd_on_prediction.R
└── output/
    ├── tables/
    ├── figures/
    ├── extension1/
    └── extension2/
```



## Data

The analysis uses one file, `data/regdata0.dta`, which is included in the repository. It comes
from the replication files published with Meyersson, E. (2014), "Islamic Rule and the
Empowerment of the Poor and Pious", *Econometrica*, 82(1), 229–269.

## Requirements

- R and RStudio. Tested with R 4.6.2. The R packages, including the archived `rdd`
  package, are installed automatically by `code/00_setup.R` the first time `run_all.R` runs.
  This needs an internet connection.
- Python with `numpy`, `pandas` and `scikit-learn`. Tested with Python 3.14.7 and
  scikit-learn 1.9.1. The course's `TECH6` conda environment has all three. Otherwise:
  `pip install numpy pandas scikit-learn`.

## How to run

1. Download the repository (green `Code` button, then `Download ZIP`, or `git clone`) and
   open the project folder.
2. Open the Anaconda Prompt (Windows) or Terminal (Mac), go to the project folder and run the
   Python part. It takes about a minute.
   ```
   cd path/to/the/project/folder
   conda activate TECH6
   python main.py
   ```
3. Open `TECH6 Collaborative Project, 19, 34, 42.Rproj` in RStudio (double-click it, or
   `File → Open Project`), so the project folder is the working directory, and run:
   ```r
   source("run_all.R")
   ```
4. The results are saved in `output/`. The script prints "Done" when it has finished.

`main.py` rebuilds `output/extension2/predictions.csv`. `run_all.R` uses the predictions in the
repository, so run `main.py` first to regenerate every result from the data.

#### Notes on extension 2:
Following an answer from the lecturer, `main.py` is run separately and is not called from `run_all.R`. 
`run_all.R` always reads the predictions in `output/extension2/` so if you only run `run_all.R`, Extension 2 uses the predictions already in our repository. If `main.py` is ran first, it overwrites `predictions.csv`, `benchmark.csv` and `accuracy.csv` so Extension 2 uses these predictions. These should be identical to what is already in the repository.

## Table and figure crosswalk

### Replication

Scripts are in `code/`. Results are saved in `output/tables/` and `output/figures/`.

| Result   | Script         | Output                         |
|----------|----------------|--------------------------------|
| Table 1  | [`01_table1.R`](code/01_table1.R)  | `table1.csv`                   |
| Table 2  | [`02_table2.R`](code/02_table2.R)  | `table2.csv`                   |
| Table 3  | [`03_table3.R`](code/03_table3.R)  | `table3.csv`                   |
| Table 7  | [`04_table7.R`](code/04_table7.R)  | `table7.csv`                   |
| Figure 1 | [`05_figure1.R`](code/05_figure1.R) | `figure1.png`                  |
| Figure 2 | [`06_figure2.R`](code/06_figure2.R) | `figure2A.png`, `figure2B.png` |
| Figure 3 | [`07_figure3.R`](code/07_figure3.R) | `figure3.png`                  |
| Figure 4 | [`08_figure4.R`](code/08_figure4.R) | `figure4.png`                  |

### Extensions

Scripts are in `code/extension1/` and `code/extension2/`, except `main.py`, which is in the
project folder. Results are saved in `output/extension1/` and `output/extension2/`.

| Result                    | Script                       | Output                                                                 | Comment               |
|---------------------------|------------------------------|------------------------------------------------------------------------|-----------------------|
| Ext. 1: sample checks     | [`01_sample.R`](code/extension1/01_sample.R) | `sample_selection.csv`, `selection_checks.csv`, `sample_characteristics.csv` |                 |
| Ext. 1: Table E1          | [`02_main_results.R`](code/extension1/02_main_results.R) | `main_results.csv`                                                     |                       |
| Ext. 1: Table E2          | [`03_control_vs_difference.R`](code/extension1/03_control_vs_difference.R) | `control_vs_difference.csv`                                            |                       |
| Ext. 1: robustness        | [`04_robustness.R`](code/extension1/04_robustness.R) | `robustness.csv`                                                       |                       |
| Ext. 2: predictions       | [`main.py`](main.py) | `predictions.csv`, `benchmark.csv`, `accuracy.csv`                     | Run before `run_all.R` |
| Ext. 2: RD on predictions | [`01_rd_on_prediction.R`](code/extension2/01_rd_on_prediction.R) | `rd_on_prediction.csv`                                                 | Reads `predictions.csv` |

## Notes

- `02_table2.R`, `04_table7.R` and the Extension 1 and 2 scripts: `fixest` notes that a
  variable was "removed because of collinearity". A province dummy, or the indicator
  `buyuk`, does not vary within the sample of a regression, so it cannot be estimated
  and is dropped.
- `03_table3.R`: five warnings that the VCOV matrix "was 'fixed'". They come from the
  cubic and quartic regressions at the narrowest bandwidths, where few towns are left
  to estimate many coefficients.

