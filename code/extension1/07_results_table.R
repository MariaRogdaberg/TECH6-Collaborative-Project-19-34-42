# ---- 1. Load the existing regression results -------------------------------
source("extension01/02_main_analysis.R")
source("extension01/03_gender_comparison.R")
source("extension01/04_controlling_vs_differencing.R")

# source() runs an existing R script, allowing us to reuse its 
# regression results without copying the code into this file.

# ---- 2. Create the main results table ---------------------------------------
models <- list(rd_1990, rd_2000, rd_change, rd_ddd, rd_control)
labels <- c(
  "Female RD 1990",
  "Female RD 2000",
  "Difference-in-discontinuities",
  "Triple differences (DDD)",
  "RDD with 1990 education control"
)

main_results <- data.frame(
  Model = labels,
  Estimate_pp = sapply(models, function(m) coef(m)[["i94"]] * 100),
  SE_pp = sapply(models, function(m) se(m)[["i94"]] * 100),
  p_value = sapply(models, function(m) pvalue(m)[["i94"]]),
  N = sapply(models, nobs)
)

main_results <- main_results |>
  mutate(
    Estimate_pp = round(Estimate_pp, 2),
    SE_pp = round(SE_pp, 2),
    p_value = round(p_value, 3)
  )

print(main_results)

# ---- 3. Save the table ------------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(main_results, "output/extension1/07_main_results.csv")