# =============================================================================
# run_all.R : runs the whole project and saves every table and figure
# ============================================================================
# TECH6 -- Data Science and Econometrics -- Term Paper
# Republication of Meyersson (2014) -- Candidates: 19, 34, 42
#
# HOW TO RUN IT
#   Open the R project (.Rproj), open this file, and run it as a whole with
#   Ctrl+Shift+Enter (Windows) / Cmd+Shift+Enter (Mac), or the Source button.
#   Output is saved in output/tables/ and output/figures/.
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

cat("\nDone: tables are saved in output/tables and figures are saved in output/figures/.\n")