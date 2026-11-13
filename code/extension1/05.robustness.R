# =============================================================================
# EXTENSION 1 — 05_robustness.R : Bandwidth Robustness
# =============================================================================
#   1. Loads Meyersson's original dataset
#   2. Constructs the common 1990–2000 sample
#   3. Estimates RD discontinuities using h/2, h and 2h
#   4. Estimates difference-in-discontinuities for each bandwidth
#   5. Compares and saves the results
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
library(fixest)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Common sample -------------------------------------------------------
# Require female education in both years.
# Create educational change and the RD interaction term.
d <- raw |>
  drop_na(hischshr1520f, c90hischshr1520f,
          iwm94, i94, pcode) |>
  mutate(
    change_f = hischshr1520f - c90hischshr1520f,
    i94_x1 = i94 * iwm94
  )

# ---- 3. Bandwidths ----------------------------------------------------------
# Original female IK bandwidth from Table II.
# Check half and twice the baseline bandwidth.
h <- 0.23976
bandwidths <- c(h / 2, h, h * 2)

# ---- 4. Estimate results for one bandwidth ---------------------------------
#  This creates a function that runs all three regressions for whichever bandwidth we provide.
run_bandwidth <- function(bw) {
  # Restrict the common sample to the bandwidth
  s <- d |>
    filter(abs(iwm94) < bw)
  
  # RD discontinuity in 1990
  rd_1990 <- feols(
    c90hischshr1520f ~ i94 + iwm94 + i94_x1,
    data = s,
    cluster = ~pcode
  )
  
  # RD discontinuity in 2000
  rd_2000 <- feols(
    hischshr1520f ~ i94 + iwm94 + i94_x1,
    data = s,
    cluster = ~pcode
  )
  
  # Change in the discontinuity
  rd_change <- feols(
    change_f ~ i94 + iwm94 + i94_x1,
    data = s,
    cluster = ~pcode
  )
  
  # Collect the estimates
  data.frame(
    Bandwidth = bw,
    N = nobs(rd_change),
    RD_1990 = coef(rd_1990)[["i94"]],
    SE_1990 = se(rd_1990)[["i94"]],
    p_value_1990 = pvalue(rd_1990)[["i94"]],
    RD_2000 = coef(rd_2000)[["i94"]],
    DiDisc = coef(rd_change)[["i94"]],
    SE_DiDisc = se(rd_change)[["i94"]],
    p_value_DiDisc = pvalue(rd_change)[["i94"]]
  )
}

# Collect the RD estimates and their statistical significance.
# We also check whether the 1990 discontinuity is significant
# at different bandwidths.

# ---- 5. Compare the three bandwidths ---------------------------------------
# Run the same analysis for h/2, h and 2h.
# runs the function three times: once for \(h/2\), once for \(h\), and once for \(2h\).
results <- lapply(bandwidths, run_bandwidth)

robustness <- bind_rows(results)

print(robustness)


# ---- 6. Save the robustness table ------------------------------------------
dir.create("output/extension1", recursive = TRUE, showWarnings = FALSE)
write_csv(robustness, "output/extension1/05_robustness.csv")