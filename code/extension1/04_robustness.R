# =============================================================================
# 04_robustness.R : Bandwidth robustness
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the extension sample, as in 02_main_results.R
#   2. Estimates the 1990 jump and the jump in the women's change at h/2, h
#      and 2h, with three sets of controls
#   3. Saves the table to output/extension1/robustness.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Covariates and province dummies -------------------------------------
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

provinces <- "factor(province_post)"

# The three sets of controls, as in 02_main_results.R
controls_table2 <- c(covariates, provinces)

controls_pre <- c("vshr_islam1994", "partycount", "merkezi", "merkezp",
                  "subbuyuk", "buyuk", "c90lpop", "c90ageshr19",
                  "c90ageshr60", "c90sexr", "c90mrf", "c90empf", provinces)

# ---- 3. Sample and bandwidth ------------------------------------------------
# As in 02_main_results.R.
d2000 <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates)))

h <- rdd::IKbandwidth(d2000$iwm94, d2000$hischshr1520f, cutpoint = 0)

d <- d2000 |>
  drop_na(c90hischshr1520f, c90hischshr1520m) |>
  mutate(i94_x1 = i94 * iwm94,
         change_f = hischshr1520f - c90hischshr1520f)

# ---- 4. One RD regression ---------------------------------------------------
run_rd <- function(y, bw, controls) {
  fit <- feols(.[y] ~ i94 + .[c("iwm94", "i94_x1", controls)],
               data = filter(d, abs(iwm94) < bw), cluster = ~pcode)
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             Bandwidth = bw,
             N = nobs(fit))
}

# One outcome and one set of controls at h/2, h and 2h
run_bandwidths <- function(label, y, controls, controls_label) {
  bind_rows(
    run_rd(y, h/2, controls),
    run_rd(y, h, controls),
    run_rd(y, h*2, controls)
  ) |>
    mutate(Outcome = label, Controls = controls_label, .before = 1)
}

# ---- 5. Both outcomes with the three sets of controls -----------------------
robustness <- bind_rows(
  run_bandwidths("Women, 1990", "c90hischshr1520f", c(), "None"),
  run_bandwidths("Women, 1990", "c90hischshr1520f", controls_table2, "Table 2"),
  run_bandwidths("Women, 1990", "c90hischshr1520f", controls_pre, "Pre-determined"),
  run_bandwidths("Women, change", "change_f", c(), "None"),
  run_bandwidths("Women, change", "change_f", controls_table2, "Table 2"),
  run_bandwidths("Women, change", "change_f", controls_pre, "Pre-determined")
)

# ---- 6. Stars and rounding --------------------------------------------------
robustness <- robustness |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         p_value = round(p_value, 3),
         Bandwidth = round(Bandwidth, 3))

# ---- 7. Save the table ------------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(robustness, "output/extension1/robustness.csv")