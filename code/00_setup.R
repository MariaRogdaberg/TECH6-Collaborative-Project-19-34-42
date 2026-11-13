# ============================================================================
# 00_setup.R : installs and checks the packages needed for the project
# ============================================================================
# WHAT THIS SCRIPT DOES
#   1. Installs every package needed to run the term paper.
#      (which includes the course packages and rdd for the bandwidths)
#   2. Checks that everything loads.
#   3. Prints a green light.
# ============================================================================

# ---- 1. Install the packages -----------------------------------------------

course_packages <- c(
  "tidyverse",   # data wrangling + plotting (dplyr, ggplot2, tidyr, ...)
  "estimatr",    # lm_robust(): regression with robust SEs -- our house style
  "fixest",      # feols(): fixed effects, IV, clustered SEs, event studies
  "causaldata",  # the datasets used in the Mixtape and in our labs
  "MatchIt",     # matching estimators 
  "cobalt",      # balance diagnostics and love plots
  "rdrobust",    # regression discontinuity estimation + plots
  "rddensity"    # manipulation (density) test at a cutoff 
)

# Only install what is missing, so re-running this script is always safe:
missing <- setdiff(course_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing)
} else {
  cat("All course packages are already installed.\n")
}

# Installing rdd from the CRAN archive.
# It includes IKbandwidth(), which is what the paper uses.
if (!requireNamespace("rdd", quietly = TRUE)) {
  rdd_needs   <- c("sandwich", "lmtest", "AER", "Formula")
  missing_rdd <- setdiff(rdd_needs, rownames(installed.packages()))
  if (length(missing_rdd) > 0) install.packages(missing_rdd)
  install.packages(
    "https://cran.r-project.org/src/contrib/Archive/rdd/rdd_0.57.tar.gz",
    repos = NULL, type = "source"
  )
}

# ---- 2. Check that every package loads -------------------------------------
all_packages <- c(course_packages, "rdd")    # rdd included in check
ok <- sapply(all_packages, function(p) requireNamespace(p, quietly = TRUE))
print(ok)

# ---- 3. The green light ----------------------------------------------------
if (all(ok)) {
  cat("Setup complete: all packages are installed\n")
} else {
  stop("These packages did not install: ",
       paste(names(ok)[!ok], collapse = ", "))
}

# ---- 4. Version information ------------------------------------------------
print(R.version.string)