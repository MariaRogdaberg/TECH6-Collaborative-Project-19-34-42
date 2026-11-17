# =============================================================================
# 01_rd_on_prediction.R : Extension 2, how much of the jump do pre-1994
# differences explain?
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Loads the actual outcome and the predictions made by main.py (Python,
#      run it first)
#   2. Uses Table 2's sample to compute the women's IK bandwidth
#   3. Estimates the jump at the cutoff in the actual outcome and in each
#      prediction, with a 95% confidence interval
#   4. Saves the table to output/extension2/rd_on_prediction.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# The predictions come from main.py, which must be run first
if (!file.exists("output/extension2/predictions.csv")) {
  stop("Run main.py first (Python, TECH6 environment)")
}

predictions <- read_csv("output/extension2/predictions.csv")

# ---- 2. Covariates ----------------------------------------------------------
# Table 2's covariates, used only to define Table 2's sample.
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

# ---- 3. Samples and bandwidth -----------------------------------------------
# Table 2's sample and its women's IK bandwidth (paper: 0.240), so the test
# looks at the same window of towns as the paper's effect.
# The regressions use main.py's towns (those with every pre-1994 feature):
# 576 within the bandwidth, so the actual jump is 0.030 rather than 0.032.
d2000 <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates)))

h <- rdd::IKbandwidth(d2000$iwm94, d2000$hischshr1520f, cutpoint = 0)

d <- predictions |>
  mutate(i94_x1 = i94 * iwm94)

# ---- 4. One RD regression ---------------------------------------------------
# As in Extension 1: linear on each side of the cutoff, within the bandwidth,
# clustered by province. No controls: a prediction built from pre-1994 data
# should not jump at the cutoff, and controls could hide a jump.
# The 95% interval is the estimate plus or minus two standard errors.
run_rd <- function(y) {
  fit <- feols(.[y] ~ i94 + iwm94 + i94_x1,
               data = filter(d, abs(iwm94) < h), cluster = ~pcode)
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             CI_low = coef(fit)[["i94"]] - 2 * se(fit)[["i94"]],
             CI_high = coef(fit)[["i94"]] + 2 * se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             N = nobs(fit))
}

# ---- 5. All rows ------------------------------------------------------------
# The first row is the actual outcome, the jump we compare against. The
# other six are the predictions from main.py. The top of each prediction's
# interval is the largest part of the actual jump that pre-1994 differences
# could account for.
rd_on_prediction <- bind_rows(
  run_rd("hischshr1520f") |> mutate(Features = "Actual", Model = "Outcome"),
  run_rd("A_OLS") |> mutate(Features = "A", Model = "OLS"),
  run_rd("A_Lasso") |> mutate(Features = "A", Model = "Lasso"),
  run_rd("A_Forest") |> mutate(Features = "A", Model = "Forest"),
  run_rd("B_OLS") |> mutate(Features = "B", Model = "OLS"),
  run_rd("B_Lasso") |> mutate(Features = "B", Model = "Lasso"),
  run_rd("B_Forest") |> mutate(Features = "B", Model = "Forest")
) |>
  relocate(Features, Model)

# ---- 6. Stars and rounding --------------------------------------------------
# Estimates as shares, rounded to three decimals like Table 2,
# with stars at the 10/5/1% significance thresholds.
rd_on_prediction <- rd_on_prediction |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         CI_low = round(CI_low, 3),
         CI_high = round(CI_high, 3),
         p_value = round(p_value, 3))

# ---- 7. Save the table ------------------------------------------------------
# Creates the output folder if needed and saves the table as CSV.
dir.create("output/extension2", recursive = TRUE, showWarnings = FALSE)
write_csv(rd_on_prediction, "output/extension2/rd_on_prediction.csv")
