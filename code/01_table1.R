# =============================================================================
# 01_table1.R : Table 1, Summary Statistics 
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Builds the sample needed for table 1
#   2. Computes mean and standard deviation for Islamic, secular and total 
#      municipalities
#   3. Computes the difference and standard errors between Islamic and secular
#   4. Saves the table to output/tables/table1.csv
# =============================================================================

# ---- 1. Loading the data ----------------------------------------------------
library(tidyverse)
raw <- haven::read_dta("data/regdata0.dta")

# ---- 2. Variables and row labels --------------------------------------------
variables <- c(
  "hischshr1520f", "hischshr1520m", "i94",
  "vshr_islam1994", "partycount", "lpop1994",
  "ageshr19", "ageshr60", "sexr", "shhs",
  "merkezi", "merkezp", "subbuyuk"
)

row_labels <- c(
  "Share women aged 15-20 with high school education",
  "Share men aged 15-20 with high school education",
  "Islamic mayor in 1994",
  "Islamic vote share 1994",
  "Number of parties receiving votes 1994",
  "Log population in 1994",
  "Population share below 19 in 2000",
  "Population share above 60 in 2000",
  "Gender ratio in 2000",
  "Household size in 2000",
  "District center",
  "Province center",
  "Sub-metro center"
)

# ---- 3. Sample --------------------------------------------------------------
# Keeping the municipalities without missing values in the 13 variables 
d <- raw |> drop_na(all_of(variables))  
islamic <- d |> filter(i94 == 1)        
secular <- d |> filter(i94 == 0)        

# ---- 4. Means and standard deviations ---------------------------------------
# for each variable v, d[[v]] is the column with that name

all_mean <- sapply(variables, function(v) mean(d[[v]]))
all_sd <- sapply(variables, function(v) sd(d[[v]]))

islamic_mean <- sapply(variables, function(v) mean(islamic[[v]]))
islamic_sd <- sapply(variables, function(v) sd(islamic[[v]]))

secular_mean <- sapply(variables, function(v) mean(secular[[v]]))
secular_sd <- sapply(variables, function(v) sd(secular[[v]]))

# ---- 5. Differences and standard errors -------------------------------------
# Regressing a variable on the binary Islamic-mayor column (i94) gives the
# difference between the averages in Islamic and secular towns, and the
# standard error. Standard error is found by plain lm(), as in the paper.

get_difference <- function(v) {
  
  # i94 defines Islamic with 1 and secular with 0, so its difference is 1.00
  # and the difference is 1 without any uncertainty, regression is skipped
  if (v == "i94") {
    return(c(Difference = 1, SE = 0, p_value = NA_real_))
  }
  
  model_data <- data.frame(y = d[[v]], IslamicMayor = d$i94)
  
  fit <- lm(y ~ IslamicMayor, data = model_data)
  regression_table <- summary(fit)$coefficients
  
  c(Difference = regression_table["IslamicMayor", "Estimate"],
    SE = regression_table["IslamicMayor", "Std. Error"],
    p_value = regression_table["IslamicMayor", "Pr(>|t|)"])
}

# One column per variable and three rows: Difference, SE, p-value
regression_results <- sapply(variables, get_difference)

# ---- 6. Significance Stars --------------------------------------------------
# Meyersson's code and published table uses 
# *** for 0.1%, ** for 1%, and * for 5% significance.

p_values <- regression_results["p_value", ]

stars <- if_else(is.na(p_values), "",
         if_else(p_values < 0.001, "***",
         if_else(p_values < 0.01,  "**",
         if_else(p_values < 0.05,  "*", ""))))

# ---- 7. Assemble the table --------------------------------------------------
table1 <- data.frame(
  Variable = row_labels,
  All_Mean = round(all_mean, 3),
  All_SD = round(all_sd, 3),
  Islamic_Mean = round(islamic_mean, 3),
  Islamic_SD = round(islamic_sd, 3),
  Secular_Mean = round(secular_mean, 3),
  Secular_SD = round(secular_sd, 3),
  Difference = round(regression_results["Difference", ], 3),
  SE = round(regression_results["SE", ], 3),
  Stars = stars
)

# Observations row: the total appears under All and Difference, 
# as in the paper's columns 1 and 4
obs_row <- data.frame(
  Variable     = "Observations",
  All_Mean     = nrow(d),
  Islamic_Mean = nrow(islamic),
  Secular_Mean = nrow(secular),
  Difference   = nrow(d)
)

table1 <- bind_rows(table1, obs_row)  # Empty cells become NA

# ---- 8. Save the table ------------------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(table1, "output/tables/table1.csv", na = "")