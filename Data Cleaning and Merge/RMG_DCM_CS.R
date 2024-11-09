## R Version: 4.3.2 

## Title: Data Cleaning and Merge Code

## Date Created: April 09, 2024
## Date Updated: June 25, 2024

## This R script has the following sections: 
# Section 1: Merging data sets 
#           Section 1.1: Importing Data sets 
#           Section 1.2: Merging Data sets
# Section 2: Data wrangling in the merged data set
#           Section 2.1: Treatment variable
#           Section 2.2: Outcome variables 
#           Section 2.3: Control variables 
# Section 3: Renaming and adding labels
#           Section 3.1: Renaming variables 
#           Section 3.2: Adding labels 
#           Section 3.3: Saving data set

## Brief Project Description: 
# The aim of the project is to evaluate the impact of a  training program rolled out by IFC and ILO in Bangladesh
# on working conditions and management practices in factory, along with worker productivity. The program trained 
# only female workers in a factory, who could then be promoted to supervisory roles.  

## Brief description of data: 
# The data used in this analysis comes from surveying the line operators and supervisors in the factory. 

## Brief variable description: 
#  The main outcomes are working conditions index and management practices index. And the treatment variable is 
# whether workers received the training or not. 

## Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  

### Clearing R environment 
rm(list=ls())

### Loading libraries in R
# If a package is not already installed then use install.packages("") to install the library first
library(haven)
library(tidyverse)
library(writexl)
library(readxl)
library(stargazer)
library(expss)
library(sandwich)
library(lmtest)
library(ltm)
library(dplyr)
library(repmis)

## Setting working directory
setwd("/Users/Desktop/Data")


########################################################################################################
############################## SECTION 1: MERGING DATA SETS ############################################
########################################################################################################

################### 1.1 IMPORTING DATA SETS

# Supervisor data
sup_data <- read_dta("clean_IFC_GEAR_trainee_comparison_sv_v3.dta")
# Line operator data
lo_data <- read_dta("clean_IFC_GEAR_line_operator_v3.dta")


################### 1.2 MERGING DATA SETS

# Adding prefix to all variables in both data sets to perform a merge later
sup_data <- sup_data %>% 
  rename_with(.fn = ~ paste0("sup_", .x)) 

lo_data <- lo_data %>% 
  rename_with(.fn = ~ paste0("lo_", .x)) 

# Renaming the unique supervisor ID variable in both data sets to perform a merge later
sup_data <- sup_data %>%
  rename(id_sup_merge=sup_res_id)

lo_data <- lo_data %>%
  rename(id_sup_merge=lo_gi_sv_unique_id)

# Merging both data sets by unique supervisor ID variable 
merged_data <- merge(sup_data, lo_data, by = "id_sup_merge")

# Dropping extra 'line code' variable 
merged_data <- merged_data %>% 
  dplyr::select(-lo_line_code)


########################################################################################################
############################## SECTION 2: DATA WRANGLING IN THE MERGED DATA SET ########################
########################################################################################################


#################### 2.1 TREATMENT VARIABLE 

####### TRAINEE STATUS 

# Recoding the treatment variable: trainee = 1 and comparison = 0
merged_data$sup_res_type <- replace(merged_data$sup_res_type, merged_data$sup_res_type==2, 0)


#################### 2.2 OUTCOME VARIABLES

####### 2.2.1 WORKING CONDITIONS INDEX 

## Creating an index to measure working conditions at the factories in the study

# Recoding values '-97' and '-99' with NA across survey response columns pertaining to working conditions
merged_data <- merged_data %>%
  mutate(across(lo_wc_3a:lo_wc_3h, ~ ifelse(.x < 0, NA, .x)))

# Reverse coding the response scale for ease of interpretation
reverse_cols <- c("lo_wc_3a", "lo_wc_3b", "lo_wc_3d", "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", 
                  "lo_wc_3h") 

merged_data[,reverse_cols] <- 6 - merged_data[ , reverse_cols]

# Creating a new variable with horizontal summation across survey response columns
merged_data$'lo_wc_3_sum' <- rowSums(merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", 
                                                    "lo_wc_3d","lo_wc_3e","lo_wc_3f", "lo_wc_3g", "lo_wc_3h")], na.rm = T)

# Relocating the new variable in the data frame
merged_data <- merged_data %>% 
  relocate(lo_wc_3_sum, .after = lo_wc_3h)

# Creating a function for standardization 
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Appling the standardization function to obtain a new variable 
merged_data$'lo_wc_3_zscore' <- standardize(merged_data$lo_wc_3_sum)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_wc_3_zscore, .after = lo_wc_3_sum)

# Calculating Cronbach's Alpha coefficient for the survey responses 

wc_response_data <- merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", "lo_wc_3d", 
                                   "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", "lo_wc_3h")]
cronbach.alpha(wc_response_data, CI = T, na.rm = T)


####### 2.2.2 MANAGEMENT PRACTICES INDEX

## Creating an index to measure management practices of supervisors at factories in the study

# Recoding values '-97' and '-99' with NA across the survey response columns pertaining to management practices
merged_data <- merged_data %>%
  mutate(across(lo_sa_3a:lo_sa_3i, ~ ifelse(.x < 0, NA, .x)))

# Creating a new variable with horizontal summation across survey response columns 
merged_data$'lo_sa_3_sum' <- rowSums(merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", 
                                                    "lo_sa_3d","lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")], na.rm = T)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_sa_3_sum, .after = lo_sa_3i)

# Creating a function for standardization
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Applying the standardization function to obtain a new variable 
merged_data$'lo_sa_3_zscore' <- standardize(merged_data$lo_sa_3_sum)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_sa_3_zscore, .after = lo_sa_3_sum)

# Calculating Cronbach's Alpha coefficient for the survey responses

mp_response_data <- merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", "lo_sa_3d",
                                   "lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")]
cronbach.alpha(mp_response_data, CI = T, na.rm = T)


#################### 2.3 CONTROL VARIABLES

####### 2.3.1 SUPERVISOR'S WORK EXPERIENCE IN MONTHS 

## Creating a function for year to month conversion 

conversion = function(x){ 
  z <- (x*12)
  return(z) 
}

# Applying the conversion function to obtain a new variable 
merged_data$'sup_wh_8_year_new' <- conversion(merged_data$sup_wh_8_year)

# Relocating the new variable in the data frame
merged_data <- merged_data %>% 
  relocate(sup_wh_8_year_new, .after = sup_wh_8_year)

# Creating a new variable for the total number of months worked by supervisors 
merged_data$'sup_wh_8_total_months' <- rowSums(merged_data[,c("sup_wh_8_year_new", "sup_wh_8_month")])

# Relocating the new variable in the data frame
merged_data <- merged_data %>% 
  relocate(sup_wh_8_total_months, .after = sup_wh_8_month)


####### 2.3.2 SUPERVISOR AND LINE OPERATOR'S SEX

## Recoding the sex variable: female = 1 and male = 0

# For supervisors
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==1, 0)
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==2, 1)

# For line operators
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==1, 0)
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==2, 1)


####### 2.3.3 SUPERVISOR AND LINE OPERATOR'S EDUCATION STATUS

## Recoding variable for certain values of 'total years of education'

# For supervisors
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 77, 0, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 15, 17, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 14, 15, merged_data$sup_di_3)

# For line operators
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 77, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 88, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 16, 10, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 15, 17, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 14, 15, merged_data$lo_di_3)


####### 2.3.4 SUPERVISOR AND LINE OPERATOR'S MARITAL STATUS

## Creating binary variables for marital status: married = 1 and unmarried = 0

# For supervisors
merged_data$'sup_di_4_new' <- ifelse(merged_data$sup_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(sup_di_4_new, .after = sup_di_4)

# For line operators
merged_data$'lo_di_4_new' <- ifelse(merged_data$lo_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_di_4_new, .after = lo_di_4)


######################################################################################################
############################## SECTION 3: RENAMING AND ADDING LABELS #################################
######################################################################################################

#################### 3.1 RENAMING VARIABLES
merged_data <- merged_data %>% 
  rename(trainee_status = sup_res_type, wc_index = lo_wc_3_zscore, mp_index = lo_sa_3_zscore, 
         sup_workexper_mths = sup_wh_8_total_months, sup_age = sup_di_1, sup_educ = sup_di_3, 
         sup_marital_status = sup_di_4_new, lo_age = lo_di_1, lo_gender = lo_di_2, 
         lo_educ = lo_di_3, lo_marital_status = lo_di_4_new, factory_code = sup_gi_4, 
         line_code = sup_line_code)

#################### 3.2 ADDING LABELS
merged_data <- apply_labels(merged_data,
                            trainee_status = "Trainee or Comparison supervisor",
                            wc_index = "Working conditions index",
                            mp_index = "Management practices index", 
                            sup_workexper_mths = "Supervisor's work experience in months", 
                            sup_age = "Supervisor's age", 
                            sup_educ = "Supervisor's education", 
                            sup_marital_status = "Supervisor's marital status", 
                            lo_age = "Line operator's age", 
                            lo_gender = "Line operator's gender", 
                            lo_educ = "Line operator's education",
                            lo_marital_status = "Line operator's marital status", 
                            line_code = "Line Code") 


#################### 3.3 SAVING DATA SET
# R
save(merged_data, file = "/Users/Desktop/Data/merged/survey_data_final.RData")
# Stata
write_dta(merged_data, "/Users/Desktop/Data/merged/survey_data_final.dta")


################################### END OF SCRIPT ###########################################