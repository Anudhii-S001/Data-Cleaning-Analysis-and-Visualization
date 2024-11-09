## R Version: 4.3.2 

## Title: Data Analysis - Nearest Neighbor Matching and Regression Analysis

## Date Created: April 26, 2024
## Date Updated: July 15, 2024

## This R script has the following sections: 
# Section 1: OLS Fixed Effects Regression before Statistical Matching 
#            Section 1.1: Outcome Variable: Working Conditions Index 
#            Section 1.2: Outcome Variable: Management Practices Index
# Section 2: OLS Fixed Effects Regression after Statistical Matching 
#            Section 2.1: Nearest Neighbor Matching
#            Section 2.2: Regression after Nearest Neighbor Matching using Mahalanobis Distance 
#            Section 2.3: Regression after Nearest Neighbor Matching using Propensity Score
# Section 3: Logistic Fixed Effects Regression  
#            Section 3.1: Individual components of Working Conditions Index  
#            Section 3.2: Individual components of Management Practices Index 
# Section 4: Regression Tables 

## Brief Project Description: 
# The aim of the project is to evaluate the impact of a  training program rolled out by IFC and ILO in Bangladesh
# on working conditions and management practices in factory, along with worker productivity. The program trained 
# only female workers in a factory, who could then be promoted to supervisory roles.  

## Brief description of data: 
# The data used in this analysis comes from surveying the line operators and supervisors in the factory. 
# For the analysis, line operator and supervisor data were merged.   

## Brief variable description: 
# Outcomes: Working Conditions Index, Management Practices Index, and individual components comprising both indices. 
# Treatment: 'Trainee Status' is the treatment variable. 

## Clustered standard errors by supervisor's ID in all regressions. 
## Factory fixed effects included in all regressions. 
## Nearest Neighboring Matching used with two distance metrics: Mahalanobis distance and Propensity score. 

## Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  

### Clearing R environment 
rm(list=ls())

### Loading libraries (if package not already installed then use install.packages() to install the library first)
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
library(MatchIt)
library(cobalt)
library(aod)

### Set working directory to import data
setwd("/Users/Desktop/Data/merged")

### Importing Data Set
merged_data <- read_dta("survey_data_final.dta")

########################################################################################################
################ SECTION 1: OLS FIXED EFFECTS REGRESSION BEFORE STATISTICAL MATCHING ###################
########################################################################################################

########################## 1.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_1)

## Same as Model 1 plus interaction with supervisor's work experience 
model_2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
              + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_2)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_1 <- vcovCL(model_1, cluster = ~ id_sup_merge)
clse_model_2 <- vcovCL(model_2, cluster = ~ id_sup_merge) 

se_model_1 <- sqrt(diag(clse_model_1))
se_model_2 <- sqrt(diag(clse_model_2))


################# ADDITIONAL REGRESSIONS WITH SAMPLE RESTRCITIONS 

########## 1.1.1 Restricting the sample to female line operators 

# Filtering for female line operators
female_lo_data <- merged_data %>% 
  filter(lo_gender==1)

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_3 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                lo_age + lo_educ + lo_marital_status  + factor(factory_code), 
              data = female_lo_data)
summary(model_3)

## Same as Model 3 plus interaction with supervisor's work experience 
model_4 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
              + lo_age + lo_educ + lo_marital_status  + factor(factory_code), 
              data = female_lo_data)
summary(model_4)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_3 <- vcovCL(model_3, cluster = ~ id_sup_merge)
clse_model_4 <- vcovCL(model_4, cluster = ~ id_sup_merge) 

se_model_3 <- sqrt(diag(clse_model_3))
se_model_4 <- sqrt(diag(clse_model_4))

########## 1.1.2 Adding a dummy controlling for female comparison supervisors 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_5 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_comp_male + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_5)

## Same as Model 5 plus interaction with supervisor's work experience 
model_6 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_comp_male + sup_age + sup_educ + 
                sup_marital_status + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
              data = merged_data)
summary(model_6)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_5 <- vcovCL(model_5, cluster = ~ id_sup_merge)
clse_model_6 <- vcovCL(model_6, cluster = ~ id_sup_merge) 

se_model_5 <- sqrt(diag(clse_model_5))
se_model_6 <- sqrt(diag(clse_model_6))


########################## 1.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX

## Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
model_7 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), data = merged_data)
summary(model_7)

## Same as Model 7 plus interaction with supervisor's work experience
model_8 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
              + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), data = merged_data)
summary(model_8)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_7 <- vcovCL(model_7, cluster = ~ id_sup_merge)
clse_model_8 <- vcovCL(model_8, cluster = ~ id_sup_merge) 

se_model_7 <- sqrt(diag(clse_model_7))
se_model_8 <- sqrt(diag(clse_model_8))


################# ADDITIONAL REGRESSIONS WITH SAMPLE RESTRCITIONS 

########## 1.2.1 Restricting the sample to female line operators 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_9 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                lo_age + lo_educ + lo_marital_status  + factor(factory_code), 
              data = female_lo_data)
summary(model_9)

## Same as Model 9 plus interaction with supervisor's work experience
model_10 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
               + lo_age + lo_educ + lo_marital_status  + factor(factory_code), 
               data = female_lo_data)
summary(model_10)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_9 <- vcovCL(model_9, cluster = ~ id_sup_merge)
clse_model_10 <- vcovCL(model_10, cluster = ~ id_sup_merge) 

se_model_9 <- sqrt(diag(clse_model_9))
se_model_10 <- sqrt(diag(clse_model_10))

########## 1.2.2 Adding a dummy controlling for female comparison supervisors 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
model_11 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_comp_male + sup_age + sup_educ + 
                 sup_marital_status + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
               data = merged_data)
summary(model_11)

## Same as Model 11 plus interaction with supervisor's work experience and supervisor characteristics
model_12 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_comp_male + sup_age + sup_educ + 
                 sup_marital_status + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
               data = merged_data)
summary(model_12)  

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_model_11 <- vcovCL(model_11, cluster = ~ id_sup_merge)
clse_model_12 <- vcovCL(model_12, cluster = ~ id_sup_merge) 

se_model_11 <- sqrt(diag(clse_model_11))
se_model_12 <- sqrt(diag(clse_model_12))


########################################################################################################
################ SECTION 2: OLS FIXED EFFECTS REGRESSION AFTER STATISTICAL MATCHING ####################
########################################################################################################

########################## 2.1 NEAREST NEIGHBOR MATCHING 

## Setting the seed to ensure same matching results are obtained each time the 'matchit' function is called
set.seed(123)

################# 2.1.1 MAHALNOBIS DISTANCE 

### Performing Nearest Neighbor Matching using Mahalanobis distance 
nn_maha_match <- matchit(trainee_status ~ sup_workexper_mths + sup_age + sup_educ, 
                         data = merged_data,
                         method = "nearest",
                         distance = "glm",
                         mahvars =  ~ sup_workexper_mths + sup_age + sup_educ,
                         discard = "control", 
                         m.order = "random",
                         estimand = "ATT", 
                         ratio = 1, 
                         replace = F)

### Extracting matched data to be used for regression later
nn_maha_data <- match.data(nn_maha_match)

### Creating a Covariate Balance Plot to assess balance on the matched variables 
v <- data.frame(old = c("sup_workexper_mths", "sup_age", "sup_educ"),
                new = c("Supervisor's work experience", "Supervisor's age", "Supervisor's education"))

jpeg("balance_plot_maha.jpeg", pointsize = 4, width=1600, height=960, res=150)
love.plot(nn_maha_match, var.order = "unadjusted",  var.names = v, drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))
dev.off()

### Test for Common Support Assumption
jpeg("commonsupport_plot_maha.jpeg", pointsize = 4, width=1600, height=960, res=150)
bal.plot(nn_maha_match, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))
dev.off()

################# 2.1.2 PROPENSITY SCORE 

### Performing Nearest Neighbor Matching using Propensity Score
nn_ps_match <- matchit(trainee_status ~ sup_workexper_mths + sup_age + sup_educ, 
                       data = merged_data,
                       method = "nearest",
                       distance = "glm",
                       mahvars =  ~ sup_workexper_mths + sup_age + sup_educ,
                       discard = "control", 
                       m.order = "random",
                       estimand = "ATT", 
                       ratio = 1, 
                       replace = F)

### Extracting matched data to be used for regression later
nn_ps_data <- match.data(nn_ps_match)

### Creating a Covariate Balance Plot to assess balance on the matched variables 
v <- data.frame(old = c("sup_workexper_mths", "sup_age", "sup_educ"),
                new = c("Supervisor's work experience", "Supervisor's age", "Supervisor's education"))

jpeg("balance_plot_ps.jpeg", pointsize = 4, width=1600, height=960, res=150)
love.plot(nn_ps_match, var.order = "unadjusted",  var.names = v, drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))
dev.off()

### Test for Common Support Assumption
jpeg("commonsupport_plot_ps.jpeg", pointsize = 4, width=1600, height=960, res=150)
bal.plot(nn_ps_match, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))
dev.off()

########################## 2.2 REGRESSION AFTER NEAREST NEIGHBOR MATCHING USING MAHALANOBIS DISTANCE

################# 2.2.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
nn_maha_wc_att_mod1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                            lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code),
                          data = nn_maha_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_maha_wc_att_mod2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                          + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                          data = nn_maha_data, weights = weights)


## Calculating clustered standard errors for both models  (by supervisor's ID)
nn_maha_wc_mod1_v <- vcovCL(nn_maha_wc_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_maha_wc_mod2_v <- vcovCL(nn_maha_wc_att_mod2, cluster = ~ id_sup_merge + subclass) 

nn_maha_wc_mod1_se <- sqrt(diag(nn_maha_wc_mod1_v))
nn_maha_wc_mod2_se <- sqrt(diag(nn_maha_wc_mod2_v))

################# 2.2.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX 

## Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
nn_maha_mp_att_mod1 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                            lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code),
                          data = nn_maha_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_maha_mp_att_mod2 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                          + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                          data = nn_maha_data, weights = weights)

## Calculating clustered standard errors for both models  (by supervisor's ID)
nn_maha_mp_mod1_v <- vcovCL(nn_maha_mp_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_maha_mp_mod2_v <- vcovCL(nn_maha_mp_att_mod2, cluster = ~ id_sup_merge + subclass) 

nn_maha_mp_mod1_se <- sqrt(diag(nn_maha_mp_mod1_v))
nn_maha_mp_mod2_se <- sqrt(diag(nn_maha_mp_mod2_v))

########################## 2.3 REGRESSION AFTER NEAREST NEIGHBOR MATCHING USING PROPENSITY SCORE

################# 2.3.1 OUTCOME VARIABLE: WORKING CONDITIONS INDEX 

## Regression of Trainee Status on Operator Reported Outcomes of Working Conditions
nn_ps_wc_att_mod1 <- lm(wc_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                          lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code),
                        data = nn_ps_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_ps_wc_att_mod2 <- lm(wc_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                        + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                        data = nn_ps_data, weights = weights)

## Calculating clustered standard errors for both models  (by supervisor's ID)
nn_ps_wc_mod1_v <- vcovCL(nn_ps_wc_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_ps_wc_mod2_v <- vcovCL(nn_ps_wc_att_mod2, cluster = ~ id_sup_merge + subclass) 

nn_ps_wc_mod1_se <- sqrt(diag(nn_ps_wc_mod1_v))
nn_ps_wc_mod2_se <- sqrt(diag(nn_ps_wc_mod2_v))

################# 2.3.2 OUTCOME VARIABLE: MANAGEMENT PRACTICES INDEX 

## Regression of Trainee Status of Supervisors on Operator Reported Outcomes of Management Practices
nn_ps_mp_att_mod1 <- lm(mp_index ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                          lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code),
                        data = nn_ps_data, weights = weights)

## Same as Model 1 plus interaction with supervisor's work experience 
nn_ps_mp_att_mod2 <- lm(mp_index ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                        + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                        data = nn_ps_data, weights = weights)

## Calculating clustered standard errors for both models  (by supervisor's ID)
nn_ps_mp_mod1_v <- vcovCL(nn_ps_mp_att_mod1, cluster = ~ id_sup_merge + subclass)
nn_ps_mp_mod2_v <- vcovCL(nn_ps_mp_att_mod2, cluster = ~ id_sup_merge + subclass) 

nn_ps_mp_mod1_se <- sqrt(diag(nn_ps_mp_mod1_v))
nn_ps_mp_mod2_se <- sqrt(diag(nn_ps_mp_mod2_v))


########################################################################################################
########################### SECTION 3: LOGISTIC FIXED EFFECTS REGRESSION ###############################
########################################################################################################

########################## 3.1 INDIVIDUAL COMPONENTS OF WORKING CONDITIONS INDEX 

############### 3.1.1 Compared to typical supervisor in factory, to what extent did or does your supervisor give extra support to less skilled operators?

## Converting into a binary variable for logistic regression
merged_data$lo_wc_3a <- ifelse(merged_data$lo_wc_3a < 5, 0, 1)

## Converting into a factor and adding labels to factor levels 
merged_data$'lo_wc_3a_bin' <- factor(merged_data$lo_wc_3a, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_1 <- glm(lo_wc_3a_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                       lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_1)

## Same as Model 1 plus interaction with supervisor's work experience 
logit_model_2 <- glm(lo_wc_3a_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                     + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_2)  

## Wald Ch Sq Test for both models
wt_model_1 <- round(wald.test(b = coef(logit_model_1), Sigma = vcov(logit_model_1), Terms = 2:length(coef(logit_model_1)))[["result"]][["chi2"]], 3)
wt_model_2 <- round(wald.test(b = coef(logit_model_2), Sigma = vcov(logit_model_2), Terms = 2:length(coef(logit_model_2)))[["result"]][["chi2"]], 3)

## Converting estimates into odds ratio from log odds
or_logit_model_1 <- exp(coef(logit_model_1))
or_logit_model_2 <- exp(coef(logit_model_2))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_1 <- vcovCL(logit_model_1, cluster = ~ id_sup_merge)
clse_logit_model_2 <- vcovCL(logit_model_2, cluster = ~ id_sup_merge) 

se_logit_model_1 <- sqrt(diag(clse_logit_model_1))
se_logit_model_2 <- sqrt(diag(clse_logit_model_2))

## Converting into standard errors for odds ratio 
se_or_model_1 <- se_logit_model_1 * or_logit_model_1
se_or_model_2 <- se_logit_model_2 * or_logit_model_2


############### 3.1.2 Compared to typical supervisor, to what extent did or does your supervisor use praise to motivate operators?

## Converting into a binary variable for logistic regression
merged_data$lo_wc_3b <- ifelse(merged_data$lo_wc_3b < 5, 0, 1)

## Converting into a factor and adding labels to factor levels 
merged_data$'lo_wc_3b_bin' <- factor(merged_data$lo_wc_3b, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_3 <- glm(lo_wc_3b_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                       lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_3)

## Same as Model 3 plus interaction with supervisor's work experience 
logit_model_4 <- glm(lo_wc_3b_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                     + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_4)  

## Wald Ch Sq Test for both models
wt_model_3 <- round(wald.test(b = coef(logit_model_3), Sigma = vcov(logit_model_3), Terms = 2:length(coef(logit_model_3)))[["result"]][["chi2"]], 3)
wt_model_4 <- round(wald.test(b = coef(logit_model_4), Sigma = vcov(logit_model_4), Terms = 2:length(coef(logit_model_4)))[["result"]][["chi2"]], 3)

## Converting estimates into odds ratio from log odds
or_logit_model_3 <- exp(coef(logit_model_3))
or_logit_model_4 <- exp(coef(logit_model_4))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_3 <- vcovCL(logit_model_3, cluster = ~ id_sup_merge)
clse_logit_model_4 <- vcovCL(logit_model_4, cluster = ~ id_sup_merge) 

se_logit_model_3 <- sqrt(diag(clse_logit_model_3))
se_logit_model_4 <- sqrt(diag(clse_logit_model_4))

## Converting into standard errors for odds ratio 
se_or_model_3 <- se_logit_model_3 * or_logit_model_3
se_or_model_4 <- se_logit_model_4 * or_logit_model_4

############### 3.1.3 Compared to typical supervisor, to what extent did or does your supervisor use shouting or abusive language to motivate operators?

# Converting into a binary variable for logistic regression
merged_data$lo_wc_3c <- ifelse(merged_data$lo_wc_3c < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_wc_3c_bin' <- factor(merged_data$lo_wc_3c, levels = c(0,1), labels = c("Much more", "Much less"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_5 <- glm(lo_wc_3c_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                       lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_5)

## Same as Model 5 plus interaction with supervisor's work experience 
logit_model_6 <- glm(lo_wc_3c_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                     + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_6)  

## Wald Ch Sq Test for both models
wt_model_5 <- round(wald.test(b = coef(logit_model_5), Sigma = vcov(logit_model_5), Terms = 2:length(coef(logit_model_5)))[["result"]][["chi2"]], 3)
wt_model_6 <- round(wald.test(b = coef(logit_model_6), Sigma = vcov(logit_model_6), Terms = 2:length(coef(logit_model_6)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_5 <- exp(coef(logit_model_5))
or_logit_model_6 <- exp(coef(logit_model_6))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_5 <- vcovCL(logit_model_5, cluster = ~ id_sup_merge)
clse_logit_model_6 <- vcovCL(logit_model_6, cluster = ~ id_sup_merge) 

se_logit_model_5 <- sqrt(diag(clse_logit_model_5))
se_logit_model_6 <- sqrt(diag(clse_logit_model_6))

# Converting into standard errors for odds ratio 
se_or_model_5 <- se_logit_model_5 * or_logit_model_5
se_or_model_6 <- se_logit_model_6 * or_logit_model_6

############### 3.1.4 Compared to typical supervisor, to what extent did or does your supervisor involve sewing operators in solving problems on the line?

# Converting into a binary variable for logistic regression
merged_data$lo_wc_3d <- ifelse(merged_data$lo_wc_3d < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_wc_3d_bin' <- factor(merged_data$lo_wc_3d, levels = c(0,1), labels = c("Much less", "Much more"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_7 <- glm(lo_wc_3d_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                       lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_7)

## Same as Model 7 plus interaction with supervisor's work experience 
logit_model_8 <- glm(lo_wc_3d_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                     + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                     data = merged_data, family = "binomial")
summary(logit_model_8)  

## Wald Ch Sq Test for both models
wt_model_7 <- round(wald.test(b = coef(logit_model_7), Sigma = vcov(logit_model_7), Terms = 2:length(coef(logit_model_7)))[["result"]][["chi2"]], 3)
wt_model_8 <- round(wald.test(b = coef(logit_model_8), Sigma = vcov(logit_model_8), Terms = 2:length(coef(logit_model_8)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_7 <- exp(coef(logit_model_7))
or_logit_model_8 <- exp(coef(logit_model_8))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_7 <- vcovCL(logit_model_7, cluster = ~ id_sup_merge)
clse_logit_model_8 <- vcovCL(logit_model_8, cluster = ~ id_sup_merge) 

se_logit_model_7 <- sqrt(diag(clse_logit_model_7))
se_logit_model_8 <- sqrt(diag(clse_logit_model_8))

# Converting into standard errors for odds ratio 
se_or_model_7 <- se_logit_model_7 * or_logit_model_7
se_or_model_8 <- se_logit_model_8 * or_logit_model_8


########################## 3.2 INDIVIDUAL COMPONENTS OF MANAGEMENT PRACTICES INDEX 

############### 3.2.1 Compared to typical supervisor, my supervisor is more confident 

# Converting into a binary variable for logistic regression
merged_data$lo_sa_3a <- ifelse(merged_data$lo_sa_3a < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_sa_3a_bin' <- factor(merged_data$lo_sa_3a, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_9 <- glm(lo_sa_3a_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                        lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_9)

## Same as Model 9 plus interaction with supervisor's work experience 
logit_model_10 <- glm(lo_sa_3a_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                      + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_10)  

## Wald Ch Sq Test for both models
wt_model_9 <- round(wald.test(b = coef(logit_model_9), Sigma = vcov(logit_model_9), Terms = 2:length(coef(logit_model_9)))[["result"]][["chi2"]], 3)
wt_model_10 <- round(wald.test(b = coef(logit_model_10), Sigma = vcov(logit_model_10), Terms = 2:length(coef(logit_model_10)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_9 <- exp(coef(logit_model_9))
or_logit_model_10 <- exp(coef(logit_model_10))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_9 <- vcovCL(logit_model_9, cluster = ~ id_sup_merge)
clse_logit_model_10 <- vcovCL(logit_model_10, cluster = ~ id_sup_merge) 

se_logit_model_9 <- sqrt(diag(clse_logit_model_9))
se_logit_model_10 <- sqrt(diag(clse_logit_model_10))

# Converting into standard errors for odds ratio 
se_or_model_9 <- se_logit_model_9 * or_logit_model_9
se_or_model_10 <- se_logit_model_10 * or_logit_model_10

############### 3.2.2 Compared to typical supervisor, my supervisor is better at remaining calm in stressful situations

# Converting into a binary variable for logistic regression
merged_data$lo_sa_3b <- ifelse(merged_data$lo_sa_3b < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_sa_3b_bin' <- factor(merged_data$lo_sa_3b, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_11 <- glm(lo_sa_3b_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                        lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_11)

## Same as Model 11 plus interaction with supervisor's work experience 
logit_model_12 <- glm(lo_sa_3b_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                      + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_12)  

## Wald Ch Sq Test for both models
wt_model_11 <- round(wald.test(b = coef(logit_model_11), Sigma = vcov(logit_model_11), Terms = 2:length(coef(logit_model_11)))[["result"]][["chi2"]], 3)
wt_model_12 <- round(wald.test(b = coef(logit_model_12), Sigma = vcov(logit_model_12), Terms = 2:length(coef(logit_model_12)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_11 <- exp(coef(logit_model_11))
or_logit_model_12 <- exp(coef(logit_model_12))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_11 <- vcovCL(logit_model_11, cluster = ~ id_sup_merge)
clse_logit_model_12 <- vcovCL(logit_model_12, cluster = ~ id_sup_merge) 

se_logit_model_11 <- sqrt(diag(clse_logit_model_11))
se_logit_model_12 <- sqrt(diag(clse_logit_model_12))

# Converting into standard errors for odds ratio 
se_or_model_11 <- se_logit_model_11 * or_logit_model_11
se_or_model_12 <- se_logit_model_12 * or_logit_model_12

############### 3.2.3 Compared to typical supervisor, my supervisor is better at motivating operators

# Converting into a binary variable for logistic regression
merged_data$lo_sa_3c <- ifelse(merged_data$lo_sa_3c < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_sa_3c_bin' <- factor(merged_data$lo_sa_3c, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_13 <- glm(lo_sa_3c_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                        lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_13)

## Same as Model 13 plus interaction with supervisor's work experience 
logit_model_14 <- glm(lo_sa_3c_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                      + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_14)  

## Wald Ch Sq Test for both models
wt_model_13 <- round(wald.test(b = coef(logit_model_13), Sigma = vcov(logit_model_13), Terms = 2:length(coef(logit_model_13)))[["result"]][["chi2"]], 3)
wt_model_14 <- round(wald.test(b = coef(logit_model_14), Sigma = vcov(logit_model_14), Terms = 2:length(coef(logit_model_14)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_13 <- exp(coef(logit_model_13))
or_logit_model_14 <- exp(coef(logit_model_14))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_13 <- vcovCL(logit_model_13, cluster = ~ id_sup_merge)
clse_logit_model_14 <- vcovCL(logit_model_14, cluster = ~ id_sup_merge) 

se_logit_model_13 <- sqrt(diag(clse_logit_model_13))
se_logit_model_14 <- sqrt(diag(clse_logit_model_14))

# Converting into standard errors for odds ratio 
se_or_model_13 <- se_logit_model_13 * or_logit_model_13
se_or_model_14 <- se_logit_model_14 * or_logit_model_14

############### 3.2.4 Compared to typical supervisor, my supervisor is better at correcting mistakes and ensuring product quality

# Converting into a binary variable for logistic regression
merged_data$lo_sa_3d <- ifelse(merged_data$lo_sa_3d < 5, 0, 1)

# Converting into a factor and adding labels to factor levels 
merged_data$'lo_sa_3d_bin' <- factor(merged_data$lo_sa_3d, levels = c(0,1), labels = c("Disagree", "Agree"))

## Regression of Trainee Status on Operator Reported Outcomes of Working conditions
logit_model_15 <- glm(lo_sa_3d_bin ~ trainee_status + sup_workexper_mths + sup_age + sup_educ + sup_marital_status + 
                        lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_15)

## Same as Model 23 plus interaction with supervisor's work experience 
logit_model_16 <- glm(lo_sa_3d_bin ~ trainee_status*sup_workexper_mths + sup_age + sup_educ + sup_marital_status
                      + lo_age + lo_gender + lo_educ + lo_marital_status  + factor(factory_code), 
                      data = merged_data, family = "binomial")
summary(logit_model_16)  

## Wald Ch Sq Test for both models
wt_model_15 <- round(wald.test(b = coef(logit_model_15), Sigma = vcov(logit_model_15), Terms = 2:length(coef(logit_model_15)))[["result"]][["chi2"]], 3)
wt_model_16 <- round(wald.test(b = coef(logit_model_16), Sigma = vcov(logit_model_16), Terms = 2:length(coef(logit_model_16)))[["result"]][["chi2"]], 3)

# Converting estimates into odds ratio from log odds
or_logit_model_15 <- exp(coef(logit_model_15))
or_logit_model_16 <- exp(coef(logit_model_16))

## Calculating clustered standard errors for both models  (by supervisor's ID)
clse_logit_model_15 <- vcovCL(logit_model_15, cluster = ~ id_sup_merge)
clse_logit_model_16 <- vcovCL(logit_model_16, cluster = ~ id_sup_merge) 

se_logit_model_15 <- sqrt(diag(clse_logit_model_15))
se_logit_model_16 <- sqrt(diag(clse_logit_model_16))

# Converting into standard errors for odds ratio 
se_or_model_15 <- se_logit_model_15 * or_logit_model_15
se_or_model_16 <- se_logit_model_16 * or_logit_model_16


########################################################################################################
############################### SECTION 4: REGRESSION TABLES ###########################################
########################################################################################################

########################### TABLE 1: Impact of Training on Working Conditions Index - Before Matching

## Creating a list of regression models and standard errors 
wc_prematch_models <- list(model_1, model_2, model_3, model_4, model_5, model_6)
wc_prematch_se <- list(se_model_1, se_model_2, se_model_3, se_model_4, se_model_5, se_model_6)

# Filtering for control group supervisors  
control_data <- merged_data %>% 
  filter(trainee_status==0)

# Finding control mean for working conditions index 
control_means <-c(mean(control_data$wc_index, na.rm = TRUE), mean(control_data$wc_index, na.rm = TRUE), 
                  mean(control_data$wc_index, na.rm = TRUE), mean(control_data$wc_index, na.rm = TRUE),
                  mean(control_data$wc_index, na.rm = TRUE), mean(control_data$wc_index, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(wc_prematch_models,
          se = wc_prematch_se, 
          type = "html", 
          dep.var.labels.include = FALSE,
          dep.var.caption = "Standardized values of working conditions index",
          title = "Table 1: Impact of Training on Working Conditions Index - Before Matching",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", "Operator's age", "Operator's gender", 
                               "Operator's education", "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects),
          out = "wc_prematch_table.html")

########################### TABLE 2: Impact of Training on Management Practices Index - Before Matching

## Creating a list of regression models and standard errors 
mp_prematch_models <- list(model_7, model_8, model_9, model_10, model_11, model_12)
mp_prematch_se <- list(se_model_7, se_model_8, se_model_9, se_model_10, se_model_11, se_model_12)

# Finding control mean for management practices index 
control_means <-c(mean(control_data$mp_index, na.rm = TRUE), mean(control_data$mp_index, na.rm = TRUE), 
                  mean(control_data$mp_index, na.rm = TRUE), mean(control_data$mp_index, na.rm = TRUE),
                  mean(control_data$mp_index, na.rm = TRUE), mean(control_data$mp_index, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(mp_prematch_models,
          se = mp_prematch_se, 
          type = "html", 
          dep.var.labels.include = FALSE,
          dep.var.caption = "Standardized values of management practices index",
          title = "Table 2: Impact of Training on Management Practices Index - Before Matching", 
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Comparison Supervisor = Female", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", "Operator's age", "Operator's gender", 
                               "Operator's education", "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects),
          out = "mp_prematch_table.html")

########################### TABLE 3: Impact of Training on Working Conditions Index - After Matching

## Creating a list of regression models and standard errors 
wc_postmatch_models <- list(nn_maha_wc_att_mod1, nn_maha_wc_att_mod2, nn_ps_wc_att_mod1, nn_ps_wc_att_mod2)
wc_postmatch_se <- list(nn_maha_wc_mod1_se, nn_maha_wc_mod2_se,nn_ps_wc_mod1_se, nn_ps_wc_mod2_se)

# Finding control mean for working conditions index 
control_means <-c(mean(control_data$wc_index, na.rm = TRUE), mean(control_data$wc_index, na.rm = TRUE), 
                  mean(control_data$wc_index, na.rm = TRUE), mean(control_data$wc_index, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(wc_postmatch_models,
          se = wc_postmatch_se, 
          type = "html", 
          dep.var.labels.include = FALSE,
          column.labels = c("Mahalanobis", "Propensity Score"), 
          column.separate = c(2,2),
          dep.var.caption = "Standardized values of working conditions index",
          title = "Table 3: Impact of Training on Working Conditions Index - After Matching",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", "Operator's age", "Operator's gender", 
                               "Operator's education", "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects),
          out = "wc_postmatch_table.html")

########################### TABLE 4: Impact of Training on Management Practices Index - After Matching

## Creating a list of regression models and standard errors 
mp_postmatch_models <- list(nn_maha_mp_att_mod1, nn_maha_mp_att_mod2, nn_ps_mp_att_mod1, nn_ps_mp_att_mod2)
mp_postmatch_se <- list(nn_maha_mp_mod1_se, nn_maha_mp_mod2_se,nn_ps_mp_mod1_se, nn_ps_mp_mod2_se)

# Finding control mean for management practices index 
control_means <-c(mean(control_data$mp_index, na.rm = TRUE), mean(control_data$mp_index, na.rm = TRUE), 
                  mean(control_data$mp_index, na.rm = TRUE), mean(control_data$mp_index, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(mp_postmatch_models,
          se = mp_postmatch_se, 
          type = "html", 
          dep.var.labels.include = FALSE,
          column.labels = c("Mahalanobis", "Propensity Score"), 
          column.separate = c(2,2),
          dep.var.caption = "Standardized values of management practices index",
          title = "Table 4: Impact of Training on Management Practices Index - After Matching", 
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", 
                               "Supervisor's education", "Supervisor's marital status", "Operator's age", "Operator's gender", 
                               "Operator's education", "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects),
          out = "mp_postmatch_table.html")

########################### TABLE 5: Impact of Training on individual components of Working Conditions Index

## Creating a list of regression models, standard errors and coefficient estimates
wc_logit_models <- list(logit_model_1, logit_model_2, logit_model_3, logit_model_4, 
                          logit_model_5, logit_model_6, logit_model_7, logit_model_8)
wc_logit_se <- list(se_or_model_1, se_or_model_2, se_or_model_3, se_or_model_4, 
                      se_or_model_5, se_or_model_6, se_or_model_7, se_or_model_8)
wc_logit_coeff <- list(or_logit_model_1, or_logit_model_2, or_logit_model_3, or_logit_model_4, 
                         or_logit_model_5, or_logit_model_6, or_logit_model_7, or_logit_model_8)

# Finding control mean for different components of working conditions index 
control_means <-c(mean(control_data$lo_wc_3a, na.rm = TRUE), mean(control_data$lo_wc_3a, na.rm = TRUE), 
                  mean(control_data$lo_wc_3b, na.rm = TRUE), mean(control_data$lo_wc_3b, na.rm = TRUE),
                  mean(control_data$lo_wc_3c, na.rm = TRUE), mean(control_data$lo_wc_3c, na.rm = TRUE),
                  mean(control_data$lo_wc_3d, na.rm = TRUE), mean(control_data$lo_wc_3d, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(wc_logit_models,
          se = wc_logit_se, 
          coef = wc_logit_coeff,
          p.auto = FALSE, 
          type = "html", 
          dep.var.caption = "Operator Reported Outcomes of Working Conditions",
          dep.var.labels.include = FALSE,
          column.labels = c("Supervisor gives extra support to less skilled operators", 
                            "Supervisor uses praise to motivate operators",
                            "Supervisor uses less shouting or abusive language to motivate operators", 
                            "Supervisor involve sewing operators in solving problems on the line"),
          column.separate = c(2,2,2,2),
          title = "Table 5: Impact of Training on individual components of Working Conditions Index",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", "Supervisor's education", 
                               "Supervisor's marital status", "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects), 
          out = "wc_logit_table.html") 

########################### TABLE 6: Impact of Training on individual components of Management Practices Index

## Creating a list of regression models, standard errors and coefficient estimates
mp_logit_models <- list(logit_model_9, logit_model_10, logit_model_11, logit_model_12, 
                          logit_model_13, logit_model_14, logit_model_15, logit_model_16)
mp_logit_se <- list(se_or_model_9, se_or_model_10, se_or_model_11, se_or_model_12, 
                      se_or_model_13, se_or_model_14, se_or_model_15, se_or_model_16)
mp_logit_coeff <- list(or_logit_model_9, or_logit_model_10, or_logit_model_11, or_logit_model_12, 
                         or_logit_model_13, or_logit_model_14, or_logit_model_15, or_logit_model_16)

# Finding control mean for different components of management practices index 
control_means <-c(mean(control_data$lo_sa_3a, na.rm = TRUE), mean(control_data$lo_sa_3a, na.rm = TRUE), 
                  mean(control_data$lo_sa_3b, na.rm = TRUE), mean(control_data$lo_sa_3b, na.rm = TRUE),
                  mean(control_data$lo_sa_3c, na.rm = TRUE), mean(control_data$lo_sa_3c, na.rm = TRUE),
                  mean(control_data$lo_sa_3d, na.rm = TRUE), mean(control_data$lo_sa_3d, na.rm = TRUE))
control_means <- round(control_means, 3)

## Creating a row of control means for the regression table
control_means <- c("Control Mean", control_means)

## Creating a row of fixed effects for the regression table
factory_fixed_effects <- c("Factory Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

## Creating regression table using stargazer package 
stargazer(mp_logit_models,
          se = mp_logit_se, 
          coef = mp_logit_coeff,
          p.auto = FALSE, 
          type = "html", 
          dep.var.caption = "Operator Reported Outcomes of Management Practices",
          dep.var.labels.include = FALSE,
          column.labels = c("My supervisor is more confident",
                            "My supervisor is better at remaining calm in stressful situations", 
                            "My supervisor is better at motivating operators", 
                            "My supervisor is better at correcting mistakes and ensuring product quality"), 
          column.separate = c(2,2,2,2),
          title = "Table 6: Impact of Training on individual components of Management Practices Index",
          covariate.labels = c("Trainee Status", "Supervisor's work experience", "Supervisor's age", "Supervisor's education", 
                               "Supervisor's marital status", "Operator's age", "Operator's gender", "Operator's education", 
                               "Operator's marital status", "Trainee status*Supervisor's work experience"),
          omit = "factory_code",
          add.lines = list(control_means, factory_fixed_effects), 
          out = "mp_logit_table.html") 

################################### END OF SCRIPT #################################################