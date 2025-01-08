## R Version: 4.3.2 

## Title: Average Treatment Effects and Heterogeneity Analysis using Causal Forests

## Date Created: June 20, 2024
## Date Updated: August 10, 2024

## Brief Project Description: 
# The project aims to examine the impact of an early childhood development program - 'Integrated Child Development Services' -
# in India on pre-adolescent learning outcomes using data from the Indian Human Development Survey. The focus is particularly on
# the heterogeneity in treatment effects to target the intervention to subgroups that benefit the most from it. 

## Brief description of ICDS: 
# In 1975, the Government of India initiated the Integrated Child Development Services (ICDS) scheme. 
# It offers nutritional support, health assessments, referral services, immunizations, and non-formal preschool education to children aged 0 to 6 years. 

## Brief description of the Indian Human Development Survey: 
# IHDS is a nation-wide survey encompassing both rural and urban regions, comprising over 1,500 villages, 970 urban blocks and
# over 41,000 households. The survey includes a diverse range of questions on topics such as healthcare, education, employment,
# marriage, and gender relations. IHDS encompasses two waves: the first wave of survey was held in 2004–05 and the second wave
# was conducted in 2011–12. In the second wave, about 85 per cent of the households from the first wave were re-interviewed. 

## Data used for analysis: 
# For the purpose of this study, three datasets from both the waves of IHDS were combined using a common variable `household ID'
# to obtain a panel data set consisting of data from both the waves. The sample is restricted to include only those children
# whose households were part of the first wave of survey and then re-interviewed in the second wave. The information about
# whether a child received any ICDS intervention is obtained from the eligible woman in the household in the first wave and is
# matched with the learning outcomes of children in the second wave. The final dataset is a panel, comprising relevant variables
# from both the waves of the survey. However, the study is cross-sectional since the learning outcomes of children are only
# recorded in the second wave.

## Brief variable description: 
# Outcomes: Standardized Reading and Math Scores of children
# Treatment: Whether the child received any ICDS intervention

## Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  

### Clearing R environment 
rm(list=ls())

### Setting up working directory to store the output files 
setwd("/Users/Desktop/CAUSAL FOREST")

### Loading libraries in R
# If a package is not already installed then use install.packages("") to install the library first
library(tidyverse)
library(stargazer)
library(dplyr)
library(grf)
library(ggplot2)

### Loading separate data sets for reading and math scores
load("nn_ps_read_data.Rdata")
load("nn_ps_math_data.Rdata")
  
### Defining a function for the causal forest analysis
run_causal_forest <- function(data, outcome_var, treatment_var, covariate_vars) {
  
  # Setting the seed for reproducibility
  set.seed(123)
  
  # Prepare input parameters
  X <- as.matrix(data[, covariate_vars])
  Y <- as.numeric(data[[outcome_var]])
  W <- as.numeric(data[[treatment_var]])

  # Ensure there are no NAs in X, Y, W
  if (anyNA(X) || anyNA(Y) || anyNA(W)) {
    stop("Data contains NA values. Please handle missing data before proceeding.")
  }
  
  # Run models for Y and W
  Y.forest <- regression_forest(X, Y)
  Y.hat <- predict(Y.forest)$predictions
  W.forest <- regression_forest(X, W)
  W.hat <- predict(W.forest)$predictions
  
  # Raw Causal Forest with all variables
  cf.raw <- causal_forest(X, Y, W, Y.hat = Y.hat, W.hat = W.hat, num.trees = 4402)
  
  # Variable importance and selection of important variables
  varimp <- variable_importance(cf.raw)
  selected.idx <- which(varimp > mean(varimp))
  
  # Re-run Causal Forest with selected variables
  cf <- causal_forest(X[, selected.idx], Y, W, Y.hat = Y.hat, W.hat = W.hat, num.trees = 4402)
  
  # Test Overlap Assumption
  overlap_filename <- paste0("overlap_assump_", outcome_var, ".jpeg")
  jpeg(overlap_filename, pointsize = 12, width = 1900, height = 1500, res = 200)
  ggplot(data.frame(W.hat = cf$W.hat, W = factor(cf$W.orig))) + 
    geom_histogram(aes(x = W.hat, y = stat(density), fill = W), alpha = 0.3, position = "identity") +
    geom_density(aes(x = W.hat, color = W)) + 
    xlim(0, 1) +
    labs(title = paste("Causal forest propensity scores for", outcome_var),
         caption = "The propensity scores are learned via GRF's regression forest",
         x = "Propensity Score") +
    theme_minimal()
  dev.off()
  
  # Out of bag predictions
  tau.hat <- predict(cf)$predictions
  oob_cate_hist_filename <- paste0("oob_cate_hist_", outcome_var, ".jpeg")
  jpeg(oob_cate_hist_filename, pointsize = 12, width = 1900, height = 1500, res = 200)
  hist(tau.hat, main = paste("Histogram of out-of-bag CATE estimates for", outcome_var), xlab = "Estimated CATT")
  dev.off()
  
  # Best linear predictor analysis (Omnibus test for heterogeneity)
  calibration_results <- test_calibration(cf)
  
  # Function for creating ATE table
  ate_table <- function(ate, group) {
    
    # Calculate T-value and p-value
    t_value <- ate[["estimate"]] / ate[["std.err"]]
    p_value <- 2 * (1 - pnorm(abs(t_value)))  # Two-tailed test
    
    # Add significance stars based on p-value
    significance <- ifelse(p_value < 0.01, "***",
                           ifelse(p_value < 0.05, "**",
                                  ifelse(p_value < 0.1, "*", "")))
    
    # Create the Estimate column with significance stars appended
    estimate <- paste0(round(ate[["estimate"]], 3), significance)
    
    # Create a data frame with the necessary columns 
    data.frame(
      Group = group,
      Estimate = estimate,
      Std.Error = round(ate[["std.err"]], 3),
      T.value = round(t_value, 3),
      Upper.CI = round(ate[["estimate"]] + 1.96 * ate[["std.err"]], 3),
      Lower.CI = round(ate[["estimate"]] - 1.96 * ate[["std.err"]], 3)
    )
  }
  
  # Average and Heterogeneous Treatment Effect Tables
  ate_all <- ate_table(average_treatment_effect(cf), "All (Average Treatment Effect)")
  ate_female <- ate_table(average_treatment_effect(cf, subset = data$child_sex == 1), "Child's sex = Female")
  ate_male <- ate_table(average_treatment_effect(cf, subset = data$child_sex == 0), "Child's sex = Male")
  ate_public <- ate_table(average_treatment_effect(cf, subset = data$school_type1 == 1), "Child's school = Public")
  ate_private <- ate_table(average_treatment_effect(cf, subset = data$school_type1 == 2), "Child's school = Private")
  ate_mothereduc_aboveavg <- ate_table(average_treatment_effect(cf, subset = data$mother_educ > mean(data$mother_educ)), "Mother's education above average")
  ate_mothereduc_belowavg <- ate_table(average_treatment_effect(cf, subset = data$mother_educ < mean(data$mother_educ)), "Mother's education below average")
  ate_urban <- ate_table(average_treatment_effect(cf, subset = data$hh_urban == 1), "HH residence = Urban")
  ate_rural <- ate_table(average_treatment_effect(cf, subset = data$hh_urban == 0), "HH residence = Rural")
  ate_hes <- ate_table(average_treatment_effect(cf, subset = data$economic_status == 1), "HH High Economic Status")
  ate_les <- ate_table(average_treatment_effect(cf, subset = data$economic_status == 0), "HH Low Economic Status")
  ate_hindu <- ate_table(average_treatment_effect(cf, subset = data$hh_religion2 == 1), "HH Religion = Hindu")
  ate_nonhindu <- ate_table(average_treatment_effect(cf, subset = data$hh_religion2 == 0), "HH Religion = Non-Hindu")
  ate_forwardcaste <- ate_table(average_treatment_effect(cf, subset = data$hh_caste2 == 1), "HH Caste = Forward")
  ate_backwardcaste <- ate_table(average_treatment_effect(cf, subset = data$hh_caste2 == 2), "HH Caste = Backward")
  ate_othercaste <- ate_table(average_treatment_effect(cf, subset = data$hh_caste2 == 3), "HH Caste = Other")
  
  # Combine all ATE results into a single table
  ate_summary_table <- rbind(ate_all, ate_female, ate_male, ate_public, ate_private, ate_mothereduc_aboveavg, 
                             ate_mothereduc_belowavg, ate_urban, ate_rural, ate_hes, ate_les, ate_hindu, 
                             ate_nonhindu, ate_forwardcaste, ate_backwardcaste, ate_othercaste)
  
  # Define a function to rename outcome variables for the output table
  rename_outcome <- function(outcome) {
    if (outcome == "z_reading_score") return("Standardized Reading Score")
    if (outcome == "z_math_score") return("Standardized Math Score")
    
    return(outcome)  
  }
  
  # Output the ATE summary table as a stargazer formatted table
  stargazer_table_filename <- paste0("ate_summary_", outcome_var, ".html")
  stargazer(ate_summary_table, type = "html", out = stargazer_table_filename, 
            summary = FALSE, rownames = FALSE, 
            column.labels = c("Estimate", "Standard Error", "T Value", "Upper CI", "Lower CI"),
            title = paste("Average and Heterogeneous Treatment Effect Estimates:", rename_outcome(outcome_var)))
  
  # Add inline CSS for custom column widths directly to the HTML file
  cat('<style>
        th:first-child, td:first-child { width: 25%; } /* First column: 25% */
        th:not(:first-child), td:not(:first-child) { width: 15%; } /* Other columns: 15% each */
     </style>',
      file = stargazer_table_filename, append = TRUE)
  
  # Extract calibration results as a separate table
  calibration_table <- calibration_results
  
  # Return the results as a list of tables
  return(list(
    ate_summary = ate_summary_table,
    calibration = calibration_table
  ))
}


#### 1. Outcome variable - Standardized Reading Score 

# Applying the causal forest analysis function
results <- run_causal_forest(
  data = nn_ps_read_data,
  outcome_var = "z_reading_score",
  treatment_var = "icds_benef",
  covariate_vars = names(nn_ps_read_data)[!(names(nn_ps_read_data) %in% c("z_reading_score", "icds_benef"))]
)

# Printing results for Average and Heterogeneous Treatment Effects
print(results$ate_summary)

# Printing results for Omnibus Test of Heterogeneity 
print(results$calibration)

#### 2. Outcome variable - Standardized Math Score 

# Applying the causal forest analysis function
results <- run_causal_forest(
  data = nn_ps_math_data,
  outcome_var = "z_math_score",
  treatment_var = "icds_benef",
  covariate_vars = names(nn_ps_math_data)[!(names(nn_ps_math_data) %in% c("z_math_score", "icds_benef"))]
)

# Printing results for Average and Heterogeneous Treatment Effects
print(results$ate_summary)

# Printing results for Omnibus Test of Heterogeneity 
print(results$calibration)


################################### END OF SCRIPT ###########################################