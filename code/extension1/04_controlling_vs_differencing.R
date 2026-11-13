# =============================================================================
# EXTENSION 1 — 04_controlling_vs_differencing.R
# =============================================================================
#   1. Constructs the common 1990–2000 sample
#   2. Estimates the 2000 RD without 1990 controls
#   3. Estimates the 2000 RD controlling for 1990 education
#   4. Estimates difference-in-discontinuities
#   5. Compares the results using the same sample and bandwidth
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Common sample -------------------------------------------------------
# Keep the same municipalities and bandwidth across all models.
h <- 0.23976

d_rd <- raw |>
  drop_na(hischshr1520f, c90hischshr1520f,
          iwm94, i94, pcode) |>
  filter(abs(iwm94) < h) |>
  mutate(
    change_f = hischshr1520f - c90hischshr1520f,
    i94_x1 = i94 * iwm94
  )

nrow(d_rd)

# ---- 3. RD without 1990 controls --------------------------------------------
rd_original <- feols(
  hischshr1520f ~ i94 + iwm94 + i94_x1,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_original)

# ---- 4. RD controlling for 1990 education ----------------------------------
# Include female high-school completion in 1990 as a control.
# This adjusts the 2000 estimate for pre-election education levels.

rd_control <- feols(
  hischshr1520f ~ i94 + iwm94 + i94_x1 + c90hischshr1520f,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_control)

# ---- 5. Difference-in-discontinuities --------------------------------------
# Instead of including 1990 education as a control,
# use the change in female education as the outcome.
rd_difference <- feols(
  change_f ~ i94 + iwm94 + i94_x1,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_difference)

# ---- 6. Compare the estimates -----------------------------------------------
comparison <- data.frame(
  Method = c(
    "RDD without 1990 control",
    "RDD with 1990 education control",
    "Difference-in-discontinuities"
  ),
  Estimate = c(
    coef(rd_original)[["i94"]],
    coef(rd_control)[["i94"]],
    coef(rd_difference)[["i94"]]
  ),
  SE = c(
    se(rd_original)[["i94"]],
    se(rd_control)[["i94"]],
    se(rd_difference)[["i94"]]
  ),
  p_value = c(
    pvalue(rd_original)[["i94"]],
    pvalue(rd_control)[["i94"]],
    pvalue(rd_difference)[["i94"]]
  ),
  N = c(
    nobs(rd_original),
    nobs(rd_control),
    nobs(rd_difference)
  )
)

print(comparison)

# ---- 7. Save the comparison table -------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(comparison, "output/extension1/04_method_comparison.csv")