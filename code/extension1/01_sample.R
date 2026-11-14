# =============================================================================
# 01_sample.R : Sample for extension 1 and sample-selection checks
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the extension sample: Table 2's sample with 1990 education
#   2. Counts municipalities at each step of the sample selection
#   3. Checks whether having 1990 data jumps at the cutoff, and whether the
#      2000 effect differs between the full and the matched sample
#   4. Saves the tables to output/extension1/
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

controls_table2 <- c(covariates, provinces)

# ---- 3. Samples and bandwidth -----------------------------------------------
# Table 2's sample and its women's IK bandwidth (paper: 0.240).
# matched_1990 = 1 if the municipality also has 1990 education data.
d2000 <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates))) |>
  mutate(i94_x1 = i94 * iwm94,
         matched_1990 = if_else(!is.na(c90hischshr1520f) &
                                  !is.na(c90hischshr1520m), 1, 0))

h <- rdd::IKbandwidth(d2000$iwm94, d2000$hischshr1520f, cutpoint = 0)

d <- d2000 |> filter(matched_1990 == 1)          # the extension sample

# ---- 4. Sample selection table ----------------------------------------------
sample_selection <- data.frame(
  Step = c("All municipalities",
           "Table 2 sample",
           "... with 1990 education (both sexes)",
           "... within the bandwidth"),
  Municipalities = c(nrow(raw),
                     nrow(d2000),
                     nrow(d),
                     nrow(filter(d, abs(iwm94) < h)))
)

# ---- 5. Selection checks ----------------------------------------------------
# One RD regression of y within the bandwidth, with the given controls.
run_rd <- function(data, y, controls) {
  fit <- feols(.[y] ~ i94 + .[c("iwm94", "i94_x1", controls)],
               data = filter(data, abs(iwm94) < h), cluster = ~pcode)
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             N = nobs(fit))
}

selection_checks <- bind_rows(
  run_rd(d2000, "matched_1990", c()) |> mutate(Check = "Has 1990 data", Controls = "None"),
  run_rd(d2000, "matched_1990", controls_table2) |> mutate(Check = "Has 1990 data", Controls = "Table 2"),
  run_rd(d2000, "hischshr1520f", c()) |> mutate(Check = "2000 effect, Table 2 sample", Controls = "None"),
  run_rd(d2000, "hischshr1520f", controls_table2) |> mutate(Check = "2000 effect, Table 2 sample", Controls = "Table 2"),
  run_rd(d, "hischshr1520f", c()) |> mutate(Check = "2000 effect, matched sample", Controls = "None"),
  run_rd(d, "hischshr1520f", controls_table2) |> mutate(Check = "2000 effect, matched sample", Controls = "Table 2")
) |>
  relocate(Check, Controls) |>
  mutate(Estimate = round(Estimate, 3),
         SE       = round(SE, 3),
         p_value  = round(p_value, 3))

# Matched and unmatched municipalities within the bandwidth
characteristics <- d2000 |>
  filter(abs(iwm94) < h) |>
  group_by(matched_1990) |>
  summarise(N = n(),
            Islamic_mayor = round(mean(i94), 3),
            Log_population = round(mean(lpop1994), 3),
            District_center = round(mean(merkezi), 3),
            Islamic_vote = round(mean(vshr_islam1994), 3),
            Women_hs_2000 = round(mean(hischshr1520f), 3))

# ---- 6. Save the tables -----------------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(sample_selection, "output/extension1/sample_selection.csv")
write_csv(selection_checks, "output/extension1/selection_checks.csv")
write_csv(characteristics, "output/extension1/sample_characteristics.csv")