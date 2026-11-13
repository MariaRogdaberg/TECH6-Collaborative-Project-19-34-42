# =============================================================================
# 08_figure4.R : Figure 4, High school education in 2000 and 1990
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Computes the mean high school share in bins of the win margin, for
#      women and men in 2000 and in 1990
#   2. Fits a local linear line with 95% bands on each side of the cutoff
#   3. Plots the four panels and saves output/figures/figure4.png
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Outcomes and settings -----------------------------------------------
# Same method as Figure 3, but different columns
variables <- c("hischshr1520f", "hischshr1520m",
               "c90hischshr1520f", "c90hischshr1520m")

panel_labels <- c("(a) Women in 2000", "(b) Men in 2000",
                  "(c) Women in 1990", "(d) Men in 1990")

bin_width <- 0.04              # 4 percentage points
h         <- 0.08              # bandwidth of the line
range_x   <- 0.5               # the figure shows |margin| <= 0.5
bin_range <- 13 * bin_width    # = 0.52: the outermost bin is averaged in full

# ---- 3. Bin means ----------------------------------------------------------
# Bins of 4 points from the cutoff outwards on each side
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

# ---- 4. The fitted line and confidence bands -------------------------------
# As in Figure 3, but within |margin| <= 0.5, like Meyersson's code
smooth_side <- function(v, side) {
  
  s <- raw |>
    filter(!is.na(iwm94), !is.na(.data[[v]]), abs(iwm94) <= range_x) |>
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
# Four panels on the paper's common y-axis, so 2000 and 1990 can be compared.
bin_means <- bin_means |>
  mutate(variable = factor(variable, levels = variables, labels = panel_labels))
lines <- lines |>
  mutate(variable = factor(variable, levels = variables, labels = panel_labels))

figure4 <- ggplot(lines, aes(x = x, group = side)) +
  geom_line(aes(y = lower), color = "grey60") +
  geom_line(aes(y = upper), color = "grey60") +
  geom_line(aes(y = fit), color = "black") +
  geom_point(data = bin_means, aes(y = y), color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ variable, ncol = 2) +
  coord_cartesian(ylim = c(-0.05, 0.25)) +
  labs(x = "Islamic win margin 1994",
       y = "Share of 15-20-year-olds with high school degree") +
  theme_classic()

# ---- 6. Save the figure -----------------------------------------------------
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/figure4.png", figure4, width = 7, height = 6)