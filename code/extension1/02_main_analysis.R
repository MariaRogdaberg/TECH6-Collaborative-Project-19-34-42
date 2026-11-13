# =============================================================================
# EXTENSION 1 — 02_main_analysis.R : Main RD Analysis
# =============================================================================
#   1. Loads the original dataset
#   2. Constructs the common 1990–2000 sample
#   3. Estimates educational discontinuities in 1990 and 2000
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Common sample -------------------------------------------------------
# Same sample and bandwidth as 01_data_preparation.R
h <- 0.23976

d_rd <- raw |>
  drop_na(hischshr1520f, c90hischshr1520f,
          iwm94, i94, pcode) |>
  filter(abs(iwm94) < h) |>
  mutate(i94_x1 = i94 * iwm94)

# ---- 3. RD discontinuity in 1990 --------------------------------------------
rd_1990 <- feols(
  c90hischshr1520f ~ i94 + iwm94 + i94_x1,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_1990)

# ---- 4. RD discontinuity in 2000 --------------------------------------------
rd_2000 <- feols(
  hischshr1520f ~ i94 + iwm94 + i94_x1,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_2000)

# ---- 5. Difference-in-discontinuities --------------------------------------
# Calculate the change in female education
d_rd <- d_rd |>
  mutate(change_f = hischshr1520f - c90hischshr1520f)

# Estimate the RD effect on educational changes
rd_change <- feols(
  change_f ~ i94 + iwm94 + i94_x1,
  data = d_rd,
  cluster = ~pcode
)

summary(rd_change)