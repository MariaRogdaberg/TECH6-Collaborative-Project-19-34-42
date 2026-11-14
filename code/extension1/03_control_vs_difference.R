# =============================================================================
# 03_control_vs_difference.R : Controlling for 1990 vs differencing
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Replicates Table 4, column 2: controlling for the 1990 census
#   2. Compares controlling for 1990 female education with differencing it out 
#      on the extension's sample, with the same pre-determined controls
#   3. Saves the comparison to output/extension1/control_vs_difference.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Controls ------------------------------------------------------------
# Table 2's covariates define the extension sample, like in 02_main_results.R.
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

provinces <- "factor(province_post)"

# Pre-determined controls, as in 02_main_results.R: the same in both
# extension rows, so they differ only in how 1990 education enters.
controls_pre <- c("vshr_islam1994", "partycount", "merkezi", "merkezp",
                  "subbuyuk", "buyuk", "c90lpop", "c90ageshr19",
                  "c90ageshr60", "c90sexr", "c90mrf", "c90empf", provinces)

# Table 4, column 2's controls (Meyersson's code): the 1990 census, including
# 1990 education, instead of the 2000 census variables.
controls_1990 <- c("merkezi", "merkezp", "subbuyuk", "vshr_islam1994", "anyc",
                   "c90hischshr1520f", "c90hischshr1520m", "c90empf",
                   "c90mrf", "c90sexr", "c90ageshr19", "c90ageshr60",
                   "c90lpop")

# ---- 3. Samples and bandwidths ----------------------------------------------
# Table 4, column 2: its own sample and IK bandwidth (paper: 0.305).
d_t4 <- raw |>
  drop_na(all_of(c("hischshr1520f", "iwm94", "i94", "pcode", controls_1990))) |>
  mutate(i94_x1 = i94 * iwm94)

h_t4 <- rdd::IKbandwidth(d_t4$iwm94, d_t4$hischshr1520f, cutpoint = 0)

# Extension 1: Table 2's sample and bandwidth, with 1990 data.
# Every town in the extension sample has these controls
d2000 <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates)))

h <- rdd::IKbandwidth(d2000$iwm94, d2000$hischshr1520f, cutpoint = 0)

d <- d2000 |>
  drop_na(all_of(controls_1990)) |>
  mutate(i94_x1 = i94 * iwm94,
         change_f = hischshr1520f - c90hischshr1520f)

# ---- 4. One RD regression ---------------------------------------------------
run_rd <- function(data, y, h, controls) {
  fit <- feols(.[y] ~ i94 + .[c("iwm94", "i94_x1", controls)],
               data = filter(data, abs(iwm94) < h), cluster = ~pcode)
  data.frame(Estimate  = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             Bandwidth = h,
             N = nobs(fit))
}

# ---- 5. The comparison ------------------------------------------------------
comparison <- bind_rows(
  run_rd(d_t4, "hischshr1520f", h_t4, c(controls_1990, provinces)) |>
    mutate(Approach = "Table 4 col. 2 (paper's specification and sample)"),
  run_rd(d, "hischshr1520f", h, c(controls_pre, "c90hischshr1520f")) |>
    mutate(Approach = "Controlling for 1990 female education"),
  run_rd(d, "change_f", h, controls_pre) |>
    mutate(Approach = "Differencing out 1990 female education")
) |>
  relocate(Approach)

# ---- 6. Stars and rounding --------------------------------------------------
comparison <- comparison |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         p_value = round(p_value, 3),
         Bandwidth = round(Bandwidth, 3))

# ---- 7. Save the table ------------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(comparison, "output/extension1/control_vs_difference.csv")