# =============================================================================
# 03_table3.R : Table 3, RD estimates for alternative specifications
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the sample and the polynomial terms of the win margin
#   2. Estimates the RD for the polynomial order from none to
#      quartic and bandwidths between 1 and 0.05, for women and men
#   3. Saves the table to output/tables/table3.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Covariates and province dummies -------------------------------------
# Same controls as Table 2
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

provinces <- "factor(province_post)"

# ---- 3. Sample and polynomial terms -----------------------------------------
# Same municipalities as Tables 1 and 2 and same polynomial terms, but adds the
# fourth power
d <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates))) |>
  mutate(x2 = iwm94^2,
         x3 = iwm94^3,
         x4 = iwm94^4,
         i94_x1 = i94 * iwm94,
         i94_x2 = i94 * x2,
         i94_x3 = i94 * x3,
         i94_x4 = i94 * x4)

# Terms for polynomial order 0 to 4
polynomial_terms <- list(
  None = c(),
  Linear = c("iwm94", "i94_x1"),
  Quadratic = c("iwm94", "i94_x1", "x2", "i94_x2"),
  Cubic = c("iwm94", "i94_x1", "x2", "i94_x2", "x3", "i94_x3"),
  Quartic = c("iwm94", "i94_x1", "x2", "i94_x2", "x3", "i94_x3", "x4", "i94_x4")
)

bandwidths <- c(1, 0.5, 0.25, 0.1, 0.05)

outcomes <- c("A: Women" = "hischshr1520f", "B: Men"   = "hischshr1520m")

# ---- 4. One cell of the table -----------------------------------------------
# Same intuition as run_column() in Table 2, adding collin.tol to keep the small
# quartic terms at narrow bandwidths, which feols() would drop.
run_cell <- function(y, terms, h) {
  
  s <- d |> filter(abs(iwm94) < h)     # strict <, as the author: N = 2,628 at h = 1
  
  fit <- feols(.[y] ~ i94 + .[c(terms, covariates, provinces)],
               data = s, cluster = ~pcode, collin.tol = 1e-14)
  
  data.frame(Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             N = nobs(fit))
}

# ---- 5. All 50 cells --------------------------------------------------------
# One regression for every panel x polynomial order x bandwidth.
results <- list()

for (panel in names(outcomes)) {
  for (order in names(polynomial_terms)) {
    for (h in bandwidths) {
      cell <- run_cell(outcomes[[panel]], polynomial_terms[[order]], h)
      results[[length(results) + 1]] <-
        data.frame(Panel = panel, Polynomial = order, Bandwidth = h, cell)
    }
  }
}

table3 <- bind_rows(results)

# ---- 6. Stars and rounding --------------------------------------------------
# 10/5/1% thresholds, as in the paper.
table3 <- table3 |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3)) |>
  select(Panel, Polynomial, Bandwidth, Estimate, SE, Stars, N)

# ---- 7. Save the table ------------------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(table3, "output/tables/table3.csv", na = "")