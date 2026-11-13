# =============================================================================
# EXTENSION 1 — 06_sample_selection.R : Sample Selection Robustness
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Loads the original dataset
#   2. Identifies municipalities with and without 1990 education data
#   3. Checks whether data availability changes at the electoral cutoff
#   4. Compares the 2000 RD using the full and matched samples
#   5. Compares observable characteristics of the two samples
#   6. Saves the results
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Identify matched and unmatched municipalities ----------------------
# Start with all municipalities that have female education in 2000.
# Do not remove missing 1990 education yet, since we want to
# investigate which municipalities would be excluded.
h <- 0.23976

d <- raw |>
  drop_na(hischshr1520f, iwm94, i94, pcode) |>
  filter(abs(iwm94) < h) |>
  mutate(
    matched_1990 = if_else(is.na(c90hischshr1520f), 0, 1),
    i94_x1 = i94 * iwm94
  )

# matched_1990 = 1: Education data available in both years.
# matched_1990 = 0: Education data available in 2000 but not 1990.
# Count matched and unmatched municipalities.
print(d |> count(matched_1990))

# Count matched and unmatched municipalities separately
# for Islamic victories (i94 = 1) and losses (i94 = 0).
print(d |> count(i94, matched_1990))

# ---- 3. Check matching at the electoral cutoff -----------------------------
# Use the same local linear RD specification as before.
# The dependent variable is now matched_1990 instead of education.
#
# The i94 coefficient measures whether the probability of
# having 1990 data changes discontinuously at the cutoff.
#
# A significant discontinuity would raise concerns about
# differences in sample selection around the cutoff.

rd_matching <- feols(
  matched_1990 ~ i94 + iwm94 + i94_x1,
  data = d,
  cluster = ~pcode
)

# Display the result of the matching RD regression.
summary(rd_matching)

# ---- 4. Compare the 2000 RD across samples ---------------------------------
# First, estimate the 2000 discontinuity using all municipalities
# with 2000 education data within the bandwidth.
rd_full <- feols(
  hischshr1520f ~ i94 + iwm94 + i94_x1,
  data = d,
  cluster = ~pcode
)

# Next, estimate the same regression using only municipalities
# that also have 1990 education data.
rd_matched <- feols(
  hischshr1520f ~ i94 + iwm94 + i94_x1,
  data = d |> filter(matched_1990 == 1),
  cluster = ~pcode
)

# The bandwidth and regression specification are identical.
# Only the sample changes.
comparison <- data.frame(
  Sample = c("Full 2000 sample", "Matched 1990-2000 sample"),
  N = c(nobs(rd_full), nobs(rd_matched)),
  Estimate = c(
    coef(rd_full)[["i94"]],
    coef(rd_matched)[["i94"]]
  ),
  SE = c(
    se(rd_full)[["i94"]],
    se(rd_matched)[["i94"]]
  ),
  p_value = c(
    pvalue(rd_full)[["i94"]],
    pvalue(rd_matched)[["i94"]]
  )
)

# Display the comparison between the two samples.
print(comparison)

# ---- 5. Compare observable characteristics ---------------------------------

# Compare characteristics recorded in or around 1994.
# This helps us see whether retained and excluded municipalities
# have different observable characteristics.
#
# lpop1994: Log population in 1994
# partycount: Number of parties receiving votes in 1994
# merkezi: Indicator for a district-center municipality
#
# na.rm = TRUE excludes missing values when calculating means.
characteristics <- d |>
  group_by(matched_1990) |>
  summarise(
    Mean_population = mean(lpop1994, na.rm = TRUE),
    Mean_parties = mean(partycount, na.rm = TRUE),
    Mean_district_center = mean(merkezi, na.rm = TRUE),
    .groups = "drop"
  )

# Display the characteristics of both samples.
print(characteristics)

# ---- 6. Save the results ----------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(comparison, "output/extension1/06_sample_comparison.csv")
write_csv(characteristics, "output/extension1/06_sample_characteristics.csv")