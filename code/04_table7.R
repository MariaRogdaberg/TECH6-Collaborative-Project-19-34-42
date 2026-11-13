# =============================================================================
# 04_table7.R : Table 7, Long-run effects on education and marriage in 2011
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds one sample per 2011 outcome and computes its bandwidth
#   2. Estimates OLS without and with controls in panel A and the local
#      linear RD at h, h/2 and 2h in panel B, for six outcomes
#   3. Saves the table to output/tables/table7.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Covariates and province dummies -------------------------------------
# Same controls as Tables 2 and 3.
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

# Fixed effects for the 81 current provinces, SEs clustered by the 67 provinces
# of 1994 like in Meyersson's code
provinces <- "factor(province_post)"

# ---- 3. One regression ------------------------------------------------------
# Same intuition as run_column() in Table 2 and run_cell() in Table 3, 
# municipalities within the bandwidth, clustered by province.
run_model <- function(s, y, h, terms) {
  
  fit <- feols(.[y] ~ i94 + .[terms],
               data = filter(s, abs(iwm94) < h), cluster = ~pcode)
  
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             Bandwidth = h,
             N = nobs(fit))
}

# ---- 4. One outcome: panels A and B -----------------------------------------
# Each outcome has its own sample, as in Meyersson's code since the 2011 
# outcomes are missing for some municipalities.
run_outcome <- function(y) {
  
  s <- raw |>
    drop_na(all_of(c(y, "iwm94", "i94", "pcode", covariates))) |>
    mutate(i94_x1 = i94 * iwm94)
  
  h <- rdd::IKbandwidth(s$iwm94, s[[y]], cutpoint = 0)
  
  controls <- c(covariates, provinces)
  rd_terms <- c("iwm94", "i94_x1", controls)
  
  # Panel A: all municipalities (bandwidth 1.01), without and with controls
  panel_a <- bind_rows(
    run_model(s, y, 1.01, c()),
    run_model(s, y, 1.01, controls)
  ) |>
    mutate(Panel = "A: Global OLS",
           Model = c("1. Unconditional", "2. Conditional"),
           Mean = mean(s[[y]]))
  
  # Panel B: local linear RD with controls at h, h/2 and 2h
  panel_b <- bind_rows(
    run_model(s, y, h, rd_terms),
    run_model(s, y, h / 2, rd_terms),
    run_model(s, y, h * 2, rd_terms)
  ) |>
    mutate(Panel = "B: Local linear RD",
           Model = c("1. Bandwidth h", "2. Bandwidth h/2", "3. Bandwidth 2h"),
           Mean  = mean(filter(s, abs(iwm94) < h)[[y]]))
  
  bind_rows(panel_a, panel_b) |> mutate(Outcome = y, .before = 1)
}

# ---- 5. All six outcomes ----------------------------------------------------
# Outcomes from the 2011 population register: high school share for ages
# 15-19 and 15-29, and unmarried share for ages 15-19, for women and men
table7 <- bind_rows(
  run_outcome("ahischshr15_19f"),
  run_outcome("ahischshr15_19m"),
  run_outcome("ahischshr15_29f"),
  run_outcome("ahischshr15_29m"),
  run_outcome("nevm15_19f"),
  run_outcome("nevm15_19m")
)

# ---- 6. Stars and rounding --------------------------------------------------
# 10/5/1% thresholds, as in the paper. Panel A's bandwidth 1.01 is shown as 1.
table7 <- table7 |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Mean = round(Mean, 3),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         Bandwidth = round(if_else(Bandwidth > 1, 1, Bandwidth), 3)) |>
  select(Outcome, Panel, Model, Mean, Estimate, SE, Stars, Bandwidth, N)

# ---- 7. Save the table ------------------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(table7, "output/tables/table7.csv", na = "")