# =============================================================================
# EXTENSION 1 — 03_gender_comparison.R, Triple Differences
# =============================================================================
#   1. Constructs a common sample for women and men
#   2. Calculates educational changes for both genders
#   3. Estimates the difference-in-discontinuities for each gender
#   4. Estimates the triple difference
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Common sample -------------------------------------------------------
# Require education data for both genders in 1990 and 2000.
h <- 0.23976

d_gender <- raw |>
  drop_na(hischshr1520f, c90hischshr1520f,
          hischshr1520m, c90hischshr1520m,
          iwm94, i94, pcode) |>
  filter(abs(iwm94) < h) |>
  mutate(
    change_f = hischshr1520f - c90hischshr1520f,
    change_m = hischshr1520m - c90hischshr1520m,
    change_gap = change_f - change_m,
    i94_x1 = i94 * iwm94
  )

nrow(d_gender)
d_gender |> count(i94)

# ---- 3. Educational change for women ---------------------------------------
rd_female <- feols(
  change_f ~ i94 + iwm94 + i94_x1,
  data = d_gender,
  cluster = ~pcode
)

summary(rd_female)

# ---- 4. Educational change for men -----------------------------------------
rd_male <- feols(
  change_m ~ i94 + iwm94 + i94_x1,
  data = d_gender,
  cluster = ~pcode
)

summary(rd_male)

# ---- 5. Triple difference --------------------------------------------------
rd_ddd <- feols(
  change_gap ~ i94 + iwm94 + i94_x1,
  data = d_gender,
  cluster = ~pcode
)

summary(rd_ddd)