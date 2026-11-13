# =============================================================================
# EXTENSION 1 — 01_data_preparation.R : Data Preparation
# =============================================================================
#   1. Loads Meyersson's original dataset
#   2. Selects municipalities with female education data in 1990 and 2000
#   3. Calculates the change in female education
#   4. Restricts the sample to the original female RD bandwidth
#   5. Reports sample sizes and saves the sample-selection table
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Sample selection ----------------------------------------------------
# Keep municipalities with female education in 2000
# and the variables needed for the RD analysis.

d2000 <- raw |>
  drop_na(hischshr1520f, iwm94, i94, pcode)

# Require female education in 1990 as well.
# We now have the same municipalities observed in both years.

d_common <- d2000 |>
  drop_na(c90hischshr1520f)

# ---- 3. Change in female education -----------------------------------------
# Calculate the change between 1990 and 2000 for each municipality.
d_common <- d_common |>
  mutate(change_f = hischshr1520f - c90hischshr1520f)

# ---- 4. RD bandwidth --------------------------------------------------------
# Use the original female IK bandwidth from Table II.
# Apply the same bandwidth to both years and their difference.
h <- 0.23976

d_rd <- d_common |>
  filter(abs(iwm94) < h)

# ---- 5. Sample sizes --------------------------------------------------------
sample_selection <- data.frame(
  Sample = c(
    "Original dataset",
    "Female education in 2000",
    "Female education in 1990 and 2000",
    "Common sample within RD bandwidth"
  ),
  Municipalities = c(
    nrow(raw),
    nrow(d2000),
    nrow(d_common),
    nrow(d_rd)
  )
)

print(sample_selection)

# Number of Islamic wins and losses within the bandwidth
print(d_rd |> count(i94))

# ---- 6. Save the sample selection table ------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(sample_selection, "output/extension1/01_sample_selection.csv")