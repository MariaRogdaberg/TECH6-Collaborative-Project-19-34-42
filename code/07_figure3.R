# =============================================================================
# 07_figure3.R : Figure 3, Balance of pre-determined characteristics
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Computes the mean of eight characteristics in bins of the win margin
#   2. Fits a local linear line with 95% bands on each side of the cutoff
#   3. Plots the eight panels and saves output/figures/figure3.png
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Characteristics and settings ----------------------------------------
# Panels in the paper's order (left to right, top to bottom).
variables <- c("vshr_islam1994", "ageshr60", "ageshr19", "lpop1994",
               "sexr", "anyc", "partycount", "shhs")

panel_labels <- c("Islamic vote share", "Age 60+", "Age 19-",
                  "Log population", "Gender", "Any center municipality",
                  "Number of parties", "Household size")

bin_width <- 0.08              # 8 percentage points, from the paper
h         <- 0.06              # bandwidth of the line, from Meyersson's code
range_x   <- 0.3               # the figure shows |margin| < 0.3
bin_range <- 4 * bin_width     # = 0.32: the outermost bin is averaged in full

# ---- 3. Bin means -----------------------------------------------------------
# Bins of 8 points from the cutoff outwards on each side gives a margin of exactly
# zero counts as left, as in the author's code.
bin_means <- list()

for (v in variables) {
  bin_means[[v]] <- raw |>
    filter(!is.na(iwm94), !is.na(.data[[v]]), abs(iwm94) < bin_range) |>
    mutate(side = if_else(iwm94 <= 0, "left", "right"),
           bin  = trunc(iwm94 / bin_width)) |>
    group_by(side, bin) |>
    summarise(x = mean(iwm94), y = mean(.data[[v]]), .groups = "drop") |>
    mutate(variable = v)
}

bin_means <- bind_rows(bin_means)

# ---- 4. The fitted lines and confidence bands -------------------------------
# Estimating a sepearate local linear trend on each side of the cutoff
smooth_side <- function(v, side) {
  
  s <- raw |>
    filter(!is.na(iwm94), !is.na(.data[[v]]), abs(iwm94) < range_x) |>
    mutate(y = .data[[v]])
  if (side == "left")  s <- s |> filter(iwm94 < 0)
  if (side == "right") s <- s |> filter(iwm94 > 0)
  
  points <- seq(min(s$iwm94), max(s$iwm94), length.out = 50)
  out    <- list()
  
  for (i in 1:50) {
    s <- s |> mutate(dist = iwm94 - points[i],
                     w    = pmax(0, 1 - (dist / h)^2 / 5))
    fit <- lm(y ~ dist, data = s, weights = w)
    out[[i]] <- data.frame(
      x   = points[i],
      fit = coef(fit)[["(Intercept)"]],
      se  = summary(fit)$coefficients["(Intercept)", "Std. Error"])
  }
  
  bind_rows(out) |> mutate(variable = v, side = side)
}

lines <- list()
for (v in variables) {
  lines[[length(lines) + 1]] <- smooth_side(v, "left")
  lines[[length(lines) + 1]] <- smooth_side(v, "right")
}

lines <- bind_rows(lines) |>
  mutate(lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

# ---- 5. The figure ----------------------------------------------------------
# One panel per characteristic, each with its own y-axis, two per row.
bin_means <- bin_means |>
  mutate(variable = factor(variable, levels = variables, labels = panel_labels))
lines <- lines |>
  mutate(variable = factor(variable, levels = variables, labels = panel_labels))

figure3 <- ggplot(lines, aes(x = x, group = side)) +
  geom_line(aes(y = lower), color = "grey60") +
  geom_line(aes(y = upper), color = "grey60") +
  geom_line(aes(y = fit), color = "black") +
  geom_point(data = bin_means, aes(y = y), color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(x = "Islamic win margin", y = NULL) +
  theme_classic()

# ---- 6. Save the figure -----------------------------------------------------
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/figure3.png", figure3, width = 6, height = 8)