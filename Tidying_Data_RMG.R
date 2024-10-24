"
PROJECT NAME: GEAR PROGRAM IN RMG SECTOR OF BANGLADESH
SUPERVISOR: DR. MAHREEN KHAN 

SUMMARY: TIDYING DATA TO CONDUCT REGRESSION ANALYSIS LATER 

R Version: 4.3.2 

THIS R SCRIPT HAS THE FOLLOWING SECTIONS: 

SECTION 1: TIDYING AND MERGING TWO DATA SETS
SECTION 2: DATA WRANGLING FOR TREATMENT, OUTCOME AND CONTROL VARIABLES IN THE MERGED DATA SET
"


### Clearing the environment 
rm(list=ls())


### Loading libraries 
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



########################## PART 1: TIYDING AND MERGING DATA SETS ################################


################### 1.1 IMPORTING DATA SETS

# Supervisor data
sup_data <- read_dta("https://www.dropbox.com/scl/fi/4qfpj4y7yws3npuv8hvrf/primary_clean_IFC_GEAR_trainee_comparison_sv.dta?rlkey=45vpkgcd8z01ei6q5fy292nfi&dl=1")

# Line operator data
lo_data <- read_dta("https://www.dropbox.com/scl/fi/qekvwhdgbpe49zm77sn6k/primary_clean_IFC_GEAR_line_operator.dta?rlkey=dq8wxd72km9vlb0guj1vqw3tl&dl=1")


################### 1.2 TIDYING DATA SETS

# Adding prefix to all variables both data sets for ease of merge later
sup_data <- sup_data %>% 
  rename_with(.fn = ~ paste0("sup_", .x)) 

lo_data <- lo_data %>% 
  rename_with(.fn = ~ paste0("lo_", .x)) 

# Renaming the unique supervisor id variable in both data sets 
sup_data <- sup_data %>%
  rename(id_sup_merge=sup_res_id)

lo_data <- lo_data %>%
  rename(id_sup_merge=lo_gi_sv_unique_id)

# Merging both data sets by unique supervisor id variable 
merged_data <- merge(sup_data, lo_data, by = "id_sup_merge")




########################## PART 2:  DATA WRANGLING FOR TREATMENT, OUTCOME AND CONTROL VARIABLES IN THE MERGED DATA SET ################################


#################### 2.1 TREATMENT VARIABLE 

### TRAINEE STATUS 

# Recoding the treatment variable to trainee = 1 and comparison = 0
merged_data$sup_res_type <- replace(merged_data$sup_res_type, merged_data$sup_res_type==2, 0)


######## 2.2 OUTCOME VARIABLES

### 2.2.1 WORKING CONDITIONS INDEX 

## Creating an index to measure working conditions at the factories in the study

# Recoding values '-97' and '-99' with NA across survey response columns pertaining to working conditions
merged_data <- merged_data %>%
  mutate(across(lo_wc_3a:lo_wc_3h, ~ ifelse(.x < 0, NA, .x)))


hist(merged_data$lo_wc_3a)
summary(merged_data$lo_wc_3a)

table(merged_data$lo_wc_3a)

# Reverse coding the response scale for ease of interpretation
reverse_cols <- c("lo_wc_3a", "lo_wc_3b", "lo_wc_3d", "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", 
                  "lo_wc_3h") 

merged_data[,reverse_cols] <- 6 - merged_data[ , reverse_cols]

# Creating a new variable with horizontal summation across survey response columns
merged_data$'lo_wc_sum' <- rowSums(merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", 
                                                  "lo_wc_3d","lo_wc_3e","lo_wc_3f", "lo_wc_3g", "lo_wc_3h")], na.rm = T)

# Relocating the new variable in the data frame
merged_data <- merged_data %>% 
  relocate(lo_wc_sum, .after = lo_wc_3h)


# Creating a function for standardization 
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Appling the standardization function to obtain a new variable 
merged_data$'lo_wc_zscore' <- standardize(merged_data$lo_wc_sum)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_wc_zscore, .after = lo_wc_sum)

which.max(merged_data$lo_wc_sum)


# Calculating Cronbach's Alpha coefficient for the survey responses 

wc_response_data <- merged_data[,c("lo_wc_3a", "lo_wc_3b", "lo_wc_3c", "lo_wc_3d", 
                                   "lo_wc_3e", "lo_wc_3f", "lo_wc_3g", "lo_wc_3h")]

cronbach.alpha(wc_response_data, CI = T, na.rm = T)



### 2.2.2 MANAGEMENT PRACTICES

## Creating an index to measure management practices of supervisors at factories in the study

# Recoding values '-97' and '-99' with NA across the survey response columns pertaining to management practices
merged_data <- merged_data %>%
  mutate(across(lo_sa_3a:lo_sa_3i, ~ ifelse(.x < 0, NA, .x)))

# Creating a new variable with horizontal summation across survey response columns 
merged_data$'lo_sa_sum' <- rowSums(merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", 
                                                  "lo_sa_3d","lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")], na.rm = T)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_sa_sum, .after = lo_sa_3i)


# Creating a function for standardization
standardize = function(x){ 
  z <- (x - mean(x)) / sd(x) 
  return(z) 
}

# Applying the standardization function to obtain a new variable 
merged_data$'lo_sa_zscore' <- standardize(merged_data$lo_sa_sum)

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_sa_zscore, .after = lo_sa_sum)


# Calculating Cronbach's Alpha coefficient for all the working conditions survey responses 

mp_response_data <- merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", "lo_sa_3d",
                                   "lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")]

cronbach.alpha(mp_response_data, CI = T, na.rm = T)



######## 2.3 CONTROL VARIABLES


### 2.3.1 SUPERVISOR'S WORK EXPERIENCE IN MONTHS 

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



### 2.3.2 LINE OPERATOR'S GENDER 

# Recoding the gender variable to female = 1 and male = 0
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==1, 0)
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==2, 1)



### 2.3.3 SUPERVISOR AND LINE OPERATOR'S EDUCATION STATUS

## Recoding variable for values of years of education

# Supervisor's data
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 77, 0, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 15, 17, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 14, 15, merged_data$sup_di_3)

# Line operator's data 
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 77, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 88, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 16, 10, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 15, 17, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 14, 15, merged_data$lo_di_3)



### 2.3.4 SUPERVISOR AND LINE OPERATOR'S MARITAL STATUS

## Creating new binary variables for marital status 

# Supervisor's data
merged_data$'sup_di_4_new' <- ifelse(merged_data$sup_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(sup_di_4_new, .after = sup_di_4)

# Line operator's data
merged_data$'lo_di_4_new' <- ifelse(merged_data$lo_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_di_4_new, .after = lo_di_4)


## Renaming the treatment, outcome and control variables 

merged_data <- merged_data %>% 
  rename(trainee_status = sup_res_type, wc_index = lo_wc_zscore, mp_index = lo_sa_zscore, 
         sup_workexper_mths = sup_wh_8_total_months, sup_age = sup_di_1, sup_educ = sup_di_3, 
         sup_marital_status = sup_di_4_new, lo_age = lo_di_1, lo_gender = lo_di_2, 
         lo_educ = lo_di_3, lo_marital_status = lo_di_4_new, factory_code = sup_gi_4)

# Adding labels to the variables
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
                            lo_marital_status = "Line operator's marital status") 


"
## Saving new data set as an RData file 
# (Change the file path based on where the merged data set should be saved. Add double inverted commas before and after the file path before running the command)
save(merged_data, file = /Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/RMG Study/Data/merged/merged_data_R.RData)

# Save merged data set as a dta file in the Data folder in dropbox
# (Change the file path based on where the merged data set should be saved. Add double inverted commas before and after the file path before running the command)
write_dta(merged_data, /Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/RMG Study/Data/merged/merged_data_stata.dta)
"







########################################################################################################
############################## REVISED SURVEY DATA SET ############################################
########################################################################################################

### Clearing the environment 
rm(list=ls())

### Loading libraries 
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

################### 2.1 IMPORTING DATA SETS

# Supervisor data
sup_data <- read_dta("https://www.dropbox.com/scl/fi/s7kn3yqiaagzhibiiaond/clean_IFC_GEAR_trainee_comparison_sv_v3.dta?rlkey=lf2wslxib5ocfdo9kgcwl31u0&dl=1")
# Line operator data
lo_data <- read_dta("https://www.dropbox.com/scl/fi/axf0cl8cterbvq06vtba0/clean_IFC_GEAR_line_operator_v3.dta?rlkey=7es86441jmkzluul1xupcdsl1&dl=1")

################### 2.2 TIDYING DATA SETS

# Adding prefix to all variables both data sets for ease of merge later
sup_data <- sup_data %>% 
  rename_with(.fn = ~ paste0("sup_", .x)) 

lo_data <- lo_data %>% 
  rename_with(.fn = ~ paste0("lo_", .x)) 

# Renaming the unique supervisor id variable in both data sets 
sup_data <- sup_data %>%
  rename(id_sup_merge=sup_res_id)

lo_data <- lo_data %>%
  rename(id_sup_merge=lo_gi_sv_unique_id)

# Merging both data sets by unique supervisor id variable 
merged_data <- merge(sup_data, lo_data, by = "id_sup_merge")

# Dropping extra line code variable 
merged_data <- merged_data %>% 
  dplyr::select(-lo_line_code)



########################## 2.3 DATA WRANGLING FOR TREATMENT, OUTCOME AND CONTROL VARIABLES IN THE MERGED DATA SET ################################

#################### 2.3.1 TREATMENT VARIABLE 

### TRAINEE STATUS 

# Recoding the treatment variable to trainee = 1 and comparison = 0
merged_data$sup_res_type <- replace(merged_data$sup_res_type, merged_data$sup_res_type==2, 0)


#################### 2.3.2 OUTCOME VARIABLES

### 2.3.2.1 WORKING CONDITIONS INDEX 

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



### 2.3.2.2 MANAGEMENT PRACTICES

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


# Calculating Cronbach's Alpha coefficient for all the working conditions survey responses 

mp_response_data <- merged_data[,c("lo_sa_3a", "lo_sa_3b", "lo_sa_3c", "lo_sa_3d",
                                   "lo_sa_3e","lo_sa_3f", "lo_sa_3g", "lo_sa_3h", "lo_sa_3i")]
cronbach.alpha(mp_response_data, CI = T, na.rm = T)


#################### 2.3.3 CONTROL VARIABLES

### 2.3.3.1 SUPERVISOR'S WORK EXPERIENCE IN MONTHS 

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



### 2.3.3.2 SUPERVISOR AND LINE OPERATOR'S GENDER

# Recoding the gender variable to female = 1 and male = 0
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==1, 0)
merged_data$sup_di_2 <- replace(merged_data$sup_di_2, merged_data$sup_di_2==2, 1)

# Recoding the gender variable to female = 1 and male = 0
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==1, 0)
merged_data$lo_di_2 <- replace(merged_data$lo_di_2, merged_data$lo_di_2==2, 1)



### 2.3.3.3 SUPERVISOR AND LINE OPERATOR'S EDUCATION STATUS

## Recoding variable for values of years of education

# Supervisor's data
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 77, 0, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 15, 17, merged_data$sup_di_3)
merged_data$sup_di_3 <- ifelse(merged_data$sup_di_3 == 14, 15, merged_data$sup_di_3)

# Line operator's data 
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 77, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 88, 0, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 16, 10, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 15, 17, merged_data$lo_di_3)
merged_data$lo_di_3 <- ifelse(merged_data$lo_di_3 == 14, 15, merged_data$lo_di_3)



### 2.3.3.4 SUPERVISOR AND LINE OPERATOR'S MARITAL STATUS

## Creating new binary variables for marital status 

# Supervisor's data
merged_data$'sup_di_4_new' <- ifelse(merged_data$sup_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(sup_di_4_new, .after = sup_di_4)

# Line operator's data
merged_data$'lo_di_4_new' <- ifelse(merged_data$lo_di_4==2, "1", "0")

# Relocating the new variable in the data frame 
merged_data <- merged_data %>% 
  relocate(lo_di_4_new, .after = lo_di_4)


## Renaming the treatment, outcome and control variables 

merged_data <- merged_data %>% 
  rename(trainee_status = sup_res_type, wc_index = lo_wc_3_zscore, mp_index = lo_sa_3_zscore, 
         sup_workexper_mths = sup_wh_8_total_months, sup_age = sup_di_1, sup_educ = sup_di_3, 
         sup_marital_status = sup_di_4_new, lo_age = lo_di_1, lo_gender = lo_di_2, 
         lo_educ = lo_di_3, lo_marital_status = lo_di_4_new, factory_code = sup_gi_4, 
         line_code = sup_line_code)

# Adding labels to the variables
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


save(merged_data, file = "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/RMG Study/Data/merged/survey_data_final.RData")

write_dta(merged_data, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/RMG Study/Data/merged/survey_data_final.dta")


################################### END OF SCRIPT ###########################################