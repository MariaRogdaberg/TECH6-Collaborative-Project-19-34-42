# =============================================================================
# 02_main_results.R : The gender gap over time, 1990 and 2000
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the extension sample: Table 2's sample with 1990 education
#   2. Estimates the jump at the cutoff in 1990, in 2000 and in the change,
#      for women, men and women minus men, with three sets of controls
#   3. Saves the main table to output/extension1/main_results.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Covariates and province dummies -------------------------------------
# Same controls and province fixed effects as Table 2.
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

provinces <- "factor(province_post)"

# Table 2's controls include 2000 census variables, measured after the
# election, but the pre-determined set uses only the 1994 election, the
# municipality types and the 1990 census. 1990 education is left out, since
# it is part of the outcomes.
controls_table2 <- c(covariates, provinces)

controls_pre <- c("vshr_islam1994", "partycount", "merkezi", "merkezp",
                  "subbuyuk", "buyuk", "c90lpop", "c90ageshr19",
                  "c90ageshr60", "c90sexr", "c90mrf", "c90empf", provinces)

# ---- 3. Sample, outcomes and bandwidth --------------------------------------
# Using table 2's sample and its women's IK bandwidth and keeping towns with
# 1990 education for both sexes. Applying the same sample and bandwidth in 
# every row, so 1990 and 2000 can be compared and subtracted
d2000 <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates)))

h <- rdd::IKbandwidth(d2000$iwm94, d2000$hischshr1520f, cutpoint = 0)

d <- d2000 |>
  drop_na(c90hischshr1520f, c90hischshr1520m) |>
  mutate(i94_x1 = i94 * iwm94,
         change_f = hischshr1520f - c90hischshr1520f,
         change_m = hischshr1520m - c90hischshr1520m,
         gap_1990 = c90hischshr1520f - c90hischshr1520m,
         gap_2000 = hischshr1520f - hischshr1520m,
         gap_change = change_f - change_m)

# ---- 4. One RD regression ---------------------------------------------------
# As run_column() in Table 2: linear on each side of the cutoff, within the
# bandwidth, clustered by province, with the given controls.
run_rd <- function(y, controls) {
  fit <- feols(.[y] ~ i94 + .[c("iwm94", "i94_x1", controls)],
               data = filter(d, abs(iwm94) < h), cluster = ~pcode)
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             N = nobs(fit))
}

# One row of the table: the same outcome with the three sets of controls
run_row <- function(year, group, y) {
  bind_rows(run_rd(y, c()) |> mutate(Controls = "None"),
            run_rd(y, controls_table2) |> mutate(Controls = "Table 2"),
            run_rd(y, controls_pre) |> mutate(Controls = "Pre-determined")) |>
    mutate(Year = year, Group = group, .before = 1)
}

# ---- 5. All rows ------------------------------------------------------------
# The main estimand is the women's change (difference-in-discontinuities);
# the women minus men change is the triple difference.
main_results <- bind_rows(
  run_row("1990", "Women", "c90hischshr1520f"),
  run_row("2000", "Women", "hischshr1520f"),
  run_row("Change", "Women", "change_f"),
  run_row("1990", "Men", "c90hischshr1520m"),
  run_row("2000", "Men", "hischshr1520m"),
  run_row("Change", "Men", "change_m"),
  run_row("1990", "Women - men", "gap_1990"),
  run_row("2000", "Women - men", "gap_2000"),
  run_row("Change", "Women - men", "gap_change")
)

# ---- 6. Stars and rounding --------------------------------------------------
# Estimates as shares, rounded to three decimals like Table 2,
# with 10/5/1% thresholds.
main_results <- main_results |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         p_value = round(p_value, 3))

# ---- 7. Save the table ------------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(main_results, "output/extension1/main_results.csv")