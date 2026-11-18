# =============================================================================
# run_all.R : runs the whole project and saves every table and figure
# ============================================================================
# TECH6 -- Data Science and Econometrics -- Term Paper
# Replication of Meyersson (2014) -- Candidates: 19, 34, 42
#
# HOW TO RUN IT
#   Open the R project (.Rproj), open this file, and run it as a whole with
#   Ctrl+Shift+Enter (Windows) / Cmd+Shift+Enter (Mac), or the Source button.
#   Output is saved in subfolders under output/
#
#   Extension 2 starts in Python. Run main.py FIRST, in the course's TECH6
#   conda environment (conda activate TECH6, then python main.py). It saves
#   the predictions that Extension 2's R script reads.
# ============================================================================

cat("\n--- Setup ---\n")
source("code/00_setup.R")

cat("\n--- Table 1 ---\n")
source("code/01_table1.R")

cat("\n--- Table 2 ---\n")
source("code/02_table2.R")

cat("\n--- Table 3 ---\n")
source("code/03_table3.R")

cat("\n--- Table 7 ---\n")
source("code/04_table7.R")

cat("\n--- Figure 1 ---\n")
source("code/05_figure1.R")

cat("\n--- Figure 2 ---\n")
source("code/06_figure2.R")

cat("\n--- Figure 3 ---\n")
source("code/07_figure3.R")

cat("\n--- Figure 4 ---\n")
source("code/08_figure4.R")

cat("\n--- Extension 1 ---\n")
source("code/extension1/01_sample.R")
source("code/extension1/02_main_results.R")
source("code/extension1/03_control_vs_difference.R")
source("code/extension1/04_robustness.R")

cat("\n--- Extension 2 ---\n")
source("code/extension2/01_rd_on_prediction.R")

cat("\nDone: results saved in output/tables/, output/figures/, output/extension1/\n    and output/extension2/.\n")