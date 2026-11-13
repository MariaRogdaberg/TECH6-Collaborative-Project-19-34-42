# =============================================================================
# 02_table2.R : Table 2, Main RD results
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the sample and the polynomial terms of the win margin
#   2. Computes the Imbens-Kalyanaraman bandwidths
#   3. Estimates the nine columns for women (panel A) and men (panel B)
#   4. Tests whether the effects for women and men are equal (panel C)
#   5. Saves the table to output/tables/table2.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Covariates and province dummies -------------------------------------
# Controls as in Meyersson's code, including household size (shhs), which the
# table note omits. Province fixed effects created as province dummies, by
# factor()

covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

provinces <- "factor(province_post)"

# ---- 3. Sample and polynomial terms -----------------------------------------
# Same municipalities as Table 1. Powers of the win margin (iwm94) and
# their interactions with i94 let the fitted line differ on each side.

d <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates))) |>
  mutate(x2 = iwm94^2,
         x3 = iwm94^3,
         i94_x1 = i94 * iwm94,
         i94_x2 = i94 * x2,
         i94_x3 = i94 * x3)

# Terms for polynomial order 0, 1, 2 and 3
polynomial_terms <- list(
  c(),
  c("iwm94", "i94_x1"),
  c("iwm94", "i94_x1", "x2", "i94_x2"),
  c("iwm94", "i94_x1", "x2", "i94_x2", "x3", "i94_x3")
)

# ---- 4. Bandwidths ----------------------------------------------------------
# Imbens-Kalyanaraman bandwidths, as in the paper, one per outcome. rdd uses
# the kernel constant 3.43754 (the author's program: 3.4375), which makes the
# men's sample one municipality larger in columns (3), (4), (7) and (8).

ik_bandwidth <- function(y) {
  s <- d |> drop_na(all_of(y))
  rdd::IKbandwidth(s$iwm94, s[[y]], cutpoint = 0)
}

h_women <- ik_bandwidth("hischshr1520f")        # paper: 0.240
h_men <- ik_bandwidth("hischshr1520m")          # paper: 0.323
h_enroll_women <- ik_bandwidth("studshr1530f")  # paper: 0.205
h_enroll_men <- ik_bandwidth("studshr1530m")    # paper: 0.230

# ---- 5. One column of the table ---------------------------------------------
# Polynomial terms, plus covariates and province dummies if covs = TRUE
get_controls <- function(poly, covs) {
  controls <- polynomial_terms[[poly + 1]]
  if (covs) controls <- c(controls, covariates, provinces)
  controls
}

# One RD regression: municipalities within the bandwidth, clustered by province
run_column <- function(column, y, h, poly, covs) {
  
  s <- d |>
    drop_na(all_of(y)) |>
    filter(abs(iwm94) < h)
  
  fit <- feols(.[y] ~ i94 + .[get_controls(poly, covs)],
               data = s, cluster = ~pcode)
  
  data.frame(Column = column,
             Estimate = coef(fit)[["i94"]],
             SE = se(fit)[["i94"]],
             p_value = pvalue(fit)[["i94"]],
             Mean = mean(s[[y]]),
             Bandwidth = h,
             R2 = r2(fit, "r2")[["r2"]],
             N = nobs(fit))
}

# ---- 6. Panels A and B ------------------------------------------------------
# Bandwidth 1.01 keeps the full sample in columns (1)-(2) (margin runs -1 to 1)

run_panel <- function(panel, y, y_enroll, h, h_enroll) {
  bind_rows(
    run_column("(1)", y, 1.01, 0,FALSE),
    run_column("(2)", y, 1.01, 0, TRUE),
    run_column("(3)", y,h, 1, FALSE),
    run_column("(4)", y, h, 1, TRUE),
    run_column("(5)", y, h / 2, 1, TRUE),
    run_column("(6)", y, h * 2, 1, TRUE),
    run_column("(7)", y, h, 2, TRUE),
    run_column("(8)", y, h, 3, TRUE),
    run_column("(9)", y_enroll, h_enroll, 1, TRUE)
  ) |>
    mutate(Panel = panel, .before = Column)
}

table2 <- bind_rows(
  run_panel("A: Women","hischshr1520f", "studshr1530f", h_women, h_enroll_women),
  run_panel("B: Men", "hischshr1520m", "studshr1530m", h_men,   h_enroll_men)
)

# ---- 7. Panel C: equal effects for women and men? ---------------------------
# Stack women and men, interact every term with a female dummy and test the
# female x i94 coefficient. Clustered SEs without the degrees-of-freedom
# correction and a z-test, as Stata's suest; reproduces the paper's p-values.

run_equality <- function(column, y_f, y_m, h_f, h_m, poly, covs) {
  
  women <- d |> drop_na(all_of(y_f)) |> filter(abs(iwm94) < h_f) |>
    mutate(y = .data[[y_f]], female = 1)
  men   <- d |> drop_na(all_of(y_m)) |> filter(abs(iwm94) < h_m) |>
    mutate(y = .data[[y_m]], female = 0)
  stacked <- bind_rows(women, men)
  
  terms <- c("i94", get_controls(poly, covs))
  formula <- as.formula(paste("y ~ female * (", paste(terms, collapse = " + "), ")"))
  
  fit <- feols(formula, data = stacked,
               vcov = vcov_cluster("pcode", ssc = ssc(adj = FALSE)))
  
  t <- tstat(fit)[["female:i94"]]
  data.frame(Column = column, p_value_equal = 2 * pnorm(-abs(t)))
}

hs_f <- "hischshr1520f"
hs_m <- "hischshr1520m"

panel_c <- bind_rows(
  run_equality("(1)", hs_f, hs_m, 1.01, 1.01, 0, FALSE),
  run_equality("(2)", hs_f, hs_m, 1.01, 1.01, 0, TRUE),
  run_equality("(3)", hs_f, hs_m, h_women, h_men, 1, FALSE),
  run_equality("(4)", hs_f, hs_m, h_women, h_men, 1, TRUE),
  run_equality("(5)", hs_f, hs_m, h_women / 2, h_men / 2, 1, TRUE),
  run_equality("(6)", hs_f, hs_m, h_women * 2, h_men * 2, 1, TRUE),
  run_equality("(7)", hs_f, hs_m, h_women, h_men, 2, TRUE),
  run_equality("(8)", hs_f, hs_m, h_women, h_men, 3, TRUE),
  run_equality("(9)", "studshr1530f", "studshr1530m",
               h_enroll_women, h_enroll_men, 1, TRUE)
)

# ---- 8. Stars and rounding --------------------------------------------------
# 10/5/1% thresholds, as in the table note and the author's code.
# Panel C compares women and men, so it is shown once, on the women's rows.

table2 <- table2 |>
  left_join(panel_c, by = "Column") |>
  mutate(Stars = if_else(p_value < 0.01, "***",
                 if_else(p_value < 0.05, "**",
                 if_else(p_value < 0.10, "*", ""))),
         p_value_equal = if_else(Panel == "A: Women", p_value_equal, NA),
         Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         Mean = round(Mean, 3),
         Bandwidth   = round(if_else(Bandwidth > 1, 1, Bandwidth), 3),
         R2 = round(R2, 2),
         p_value_equal = round(p_value_equal, 3)) |>
  select(Panel, Column, Mean, Estimate, SE, Stars, Bandwidth, R2, N,
         p_value_equal)

# ---- 9. Save the table ------------------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(table2, "output/tables/table2.csv", na = "")