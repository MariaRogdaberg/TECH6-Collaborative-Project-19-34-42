# =============================================================================
# 05_figure1.R : Figure 1, Islamic win margin and Islamic vote share
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the sample: the same municipalities as Tables 1 and 2
#   2. Plots the win margin against the vote share, with close elections in
#      black, the cutoff and the two-party line
#   3. Saves the figure to output/figures/figure1.png
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Sample --------------------------------------------------------------
covariates <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
                "ageshr60", "sexr", "shhs", "merkezi", "merkezp",
                "subbuyuk", "buyuk")

d <- raw |>
  drop_na(all_of(c("hischshr1520f", "hischshr1520m", "iwm94", "i94",
                   "pcode", covariates))) |>
  
  filter(vshr_islam1994 < 0.75)   # Only vote shares below 0,75 are plotted

# Close elections: margin within 2 percentage points of the cutoff
close <- d |> filter(abs(iwm94) < 0.02)

# ---- 3. The two-party line --------------------------------------------------
# In a two-party election, margin = 2 x vote share - 1: from (0, -1) to
# (0.75, 0.5).
two_party <- data.frame(vshr_islam1994 = c(0, 0.75),
                        iwm94 = c(-1, 0.5))

# ---- 4. The figure ----------------------------------------------------------
figure1 <- ggplot(d, aes(x = vshr_islam1994, y = iwm94)) +
  geom_point(color = "grey60") +
  geom_point(data = close, color = "black") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(data = two_party) +
  labs(x = "Islamic vote share", y = "Islamic win margin") +
  theme_classic()

# ---- 5. Save the figure -----------------------------------------------------
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/figure1.png", figure1, width = 5, height = 4)