# =============================================================================
# 06_figure2.R : Figure 2, Density of the Islamic win margin
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Panel A: histogram of the win margin over its full range
#   2. Panel B: McCrary's density test, checking if there is a jump in the 
#      number of municipalities at the cutoff. Computes its bandwidth, the 
#      estimate and the density line with 95% bands
#   3. Saves the figures to output/figures/figure2A.png and figure2B.png
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Panel A: histogram --------------------------------------------------
# Municipalities with a win margin are split into 99 bins, 
# where the height of the bars are in percent.
hist_data <- raw |> drop_na(iwm94)

figure2A <- ggplot(hist_data, aes(x = iwm94)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                 bins = 99, fill = "grey70", color = "grey20") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Islamic win margin", y = "Percent") +
  theme_classic()

# ---- 3. Panel B: sample and bins --------------------------------------------
# Municipalities restricted to contain the women's outcome and a win
# margin with absolute value < 0.6. 
d <- raw |> drop_na(hischshr1520f, iwm94) |> filter(abs(iwm94) < 0.6)

n_obs <- nrow(d)
b <- 2 * sd(d$iwm94) / sqrt(n_obs)      # McCrary's default bin width

bins <- d |>
  count(bin = floor(iwm94 / b)) |>
  complete(bin = min(bin):max(bin), fill = list(n = 0)) |>
  mutate(mid = bin * b + b / 2,             # bin midpoint
         density = n / (n_obs * b),
         right = mid >= 0,                    # side of the cutoff
         mid2 = mid^2,
         mid3 = mid^3,
         mid4 = mid^4)

# ---- 4. Bandwidth -----------------------------------------------------------
# McCrary's bandwidth rule, as in Meyersson's replication files.

side_bandwidth <- function(side, width) {
  s <- bins |> filter(right == side)
  fit <- lm(density ~ mid + mid2 + mid3 + mid4, data = s)
  co <- coef(fit)
  curvature <- 2 * co[["mid2"]] + 6 * co[["mid3"]] * s$mid +
    12 * co[["mid4"]] * s$mid^2         # second derivative
  3.348 * (summary(fit)$sigma^2 * width / sum(curvature^2))^(1/5)
}

h <- (side_bandwidth(FALSE, 0 - min(bins$mid)) +
        side_bandwidth(TRUE,  max(bins$mid))) / 2     # paper's program: 0.14165

# ---- 5. The jump at the cutoff ----------------------------------------------
# Density just left and right of the cutoff, from weighted regressions on each
# side, the jump is their log difference, from McCrary
fit_left <- lm(density ~ mid, data = bins, subset = mid < 0,
                weights = pmax(0, 1 - abs(mid) / h))
fit_right <- lm(density ~ mid, data = bins, subset = mid >= 0,
                weights = pmax(0, 1 - abs(mid) / h))

f_left <- coef(fit_left)[["(Intercept)"]]
f_right <- coef(fit_right)[["(Intercept)"]]

theta <- log(f_right) - log(f_left)                             # paper: -0.049
theta_se <- sqrt((24 / 5) / (n_obs * h) * (1 / f_right + 1 / f_left))  # paper:  0.161

# ---- 6. Panel B: density line and 95% bands ---------------------------------
# The same weighted regression centred at every bin midpoint gives the line and
# its intercept SE gives the bands. Shown for |margin| < 0.5, as in the paper.
line <- list()
for (i in 1:nrow(bins)) {
  point <- bins$mid[i]
  side  <- bins |>
    filter(right == (point >= 0)) |>
    mutate(dist = mid - point)
  fit <- lm(density ~ dist, data = side, weights = pmax(0, 1 - abs(dist) / h))
  line[[i]] <- data.frame(mid = point,
                          fit = coef(fit)[["(Intercept)"]],
                          se  = summary(fit)$coefficients["(Intercept)", "Std. Error"])
}

line <- bind_rows(line) |>
  mutate(right = mid >= 0,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  filter(abs(mid) < 0.5)

figure2B <- ggplot(line, aes(x = mid, group = right)) +
  geom_point(data = filter(bins, abs(mid) < 0.5), aes(y = density),
             color = "grey50") +
  geom_line(aes(y = fit), linewidth = 1) +
  geom_line(aes(y = lower), linetype = "dashed", color = "grey50") +
  geom_line(aes(y = upper), linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", x = -0.5, y = 0, hjust = 0, vjust = 1.5, size = 3,
           label = sprintf("Discontinuity est. = %.3f, s.e. = %.3f",
                           theta, theta_se)) +
  labs(x = "Islamic win margin 1994", y = "Density") +
  theme_classic()

# ---- 7. Save the figures ----------------------------------------------------
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/figure2A.png", figure2A, width = 5, height = 4)
ggsave("output/figures/figure2B.png", figure2B, width = 5, height = 4)