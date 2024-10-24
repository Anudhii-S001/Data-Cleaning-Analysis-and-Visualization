rm(list = ls())

# Setting working directory
setwd("C:/Users/dell/Desktop/Master's Thesis")

# Installing libraries 
library(grf)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(readxl)


load("thesisdata1.Rdata")
summary(merged2_df)

load("thesisdata2.Rdata")
summary(merged3_df)

# Standardizing raw reading and math score 

merged2_df$'z_reading_score' <- ((merged2_df$raw_reading_score) - mean(merged2_df$raw_reading_score))/sd(merged2_df$raw_reading_score)
merged2_df$'z_math_score' <- ((merged2_df$raw_math_score) - mean(merged2_df$raw_math_score))/sd(merged2_df$raw_math_score)


# Recoding variables - Household Economic Status, Caste and Religion 

merged2_df$'log_hhexpend_quintile' <- split_quantile(merged2_df$log_hhexpend, 5)

merged2_df <- merged2_df %>%
  mutate(economic_status=case_when(log_hhexpend_quintile==1 | log_hhexpend_quintile==2 |  log_hhexpend_quintile==3 ~ 0,
                        log_hhexpend_quintile==4 | log_hhexpend_quintile==5 ~ 1))


merged2_df <- merged2_df %>%
  mutate(hh_caste1=case_when(hh_caste==1 ~ 1,
                             hh_caste==2 | hh_caste==3 ~ 2,
                             hh_caste==4 ~ 3))

merged2_df <- merged2_df %>%
  mutate(hh_religion1=case_when(hh_religion==1 ~ 1,
                                hh_religion==2 | hh_religion==3 | hh_religion==4 ~ 0))


## Dropping variables 
merged2_df <- merged2_df %>%
  select(-c(raw_reading_score, raw_math_score, log_hhexpend_quintile, hh_caste, hh_religion,
            stateid, distid, hhid, personid, child_age_wave1))


## Reading and math data sets 
reading_data <- merged2_df %>% 
  select(-z_math_score) 

reading_data <- reading_data %>%
  select(z_reading_score, icds_benef, everything())

math_data <- merged2_df %>% 
  select(-z_reading_score) 

math_data <- math_data %>%
  select(z_math_score, icds_benef, everything())


##############################################################################################




set.seed(123)

############ CAUSAL FOREST ANALYSIS FOR READING OUTCOMES

### Loading data set 
load("nn_ps_read_data.Rdata")

## Setting input parameters 
X <- model.matrix(z_reading_score & icds_benef ~ ., nn_ps_read_data)[, -1]
Y <- nn_ps_read_data$z_reading_score
W <- nn_ps_read_data$icds_benef

## Running models for Y and W
Y_forest = regression_forest (X, Y)
Y_hat = predict(Y_forest)$predictions
W_forest = regression_forest (X, W)
W_hat = predict(W_forest)$predictions

## Raw Casual Forest 
cf_raw_reading <- causal_forest(X, Y, W, Y.hat = Y_hat , W.hat = W_hat, num.trees = 4402)

## Variable Importance 
varimp <- variable_importance(cf_raw_reading)
names(varimp) <- colnames(X)
sorted_varimp <- data.frame(sort(varimp, decreasing=TRUE))
selected_idx = which (varimp > mean(varimp))

## Causal Forest
cf_reading <- causal_forest(X[,selected_idx], Y, W, Y.hat = Y_hat , W.hat = W_hat, num.trees = 4402)

## Testing Overlap Assumption
ggplot(data.frame(W.hat = cf_reading$W.hat, W = factor(cf_reading$W.orig))) + 
geom_histogram(aes(x = W.hat, y = stat(density), fill = W), alpha=0.3 , position ="identity") + 
geom_density(aes(x = W.hat, color = W)) + 
xlim(0,1) + labs(title = "Causal forest propensity scores for Reading Scores",
caption = "The propensity scores are learned via GRF's regression forest", xlab("Propensity Score"))

## Out of bag predictions 
tau_hat_reading <- predict(cf_reading)$predictions
hist(tau_hat_reading, main = "Histogram of out-of-bag CATE estimates for Reading Scores", xlab = "Estimated CATT") 
 
## Omnibus test for heterogeneity 
test_calibration(cf_reading)                                                                                              

## Best Linear Projection 
best_linear_proj <- best_linear_projection(cf_reading, X)

stargazer(best_linear_proj, 
          type = "html", 
          out = "BLP-read.html",
          summary = FALSE, 
          title = "Table A: Best Linear Projection of CATT", 
          single.row = TRUE)


### ATT estimate for Reading score

att <- average_treatment_effect(cf_reading, target.sample = "treated")
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



###### HTE ACROSS COVARIATES SUBGROUP CATE 

# 1. Child's Gender 
female <- nn_ps_read_data$child_sex == 1
female <- nn_ps_read_data %>% 
  filter(child_sex == 1)

att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = female)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


male <- nn_ps_read_data$child_sex == 0
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = male)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 2. Child's school type

public <- nn_ps_read_data$school_type1 == 1
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = public)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

private <- nn_ps_read_data$school_type1 == 2
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = private)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



## 3. Mother's education 
above_avg <- nn_ps_read_data$mother_educ > mean(nn_ps_read_data$mother_educ)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = above_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

below_avg <- nn_ps_read_data$mother_educ < mean(nn_ps_read_data$mother_educ)
below_avg <- nn_ps_read_data %>%
filter(mother_educ < mean(mother_educ))

att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = below_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 4. Father's education  
above_avg <- nn_ps_read_data$father_educ > mean(nn_ps_read_data$father_educ)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = above_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

below_avg <- nn_ps_read_data$father_educ < mean(nn_ps_read_data$father_educ)
below_avg <- nn_ps_read_data %>%
  filter(father_educ < mean(father_educ))

att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = below_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 5. Toilet available 
toilet <- nn_ps_read_data$hh_toilet_avail == 1
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = toilet)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

notoilet <- nn_ps_read_data$hh_toilet_avail == 0
notoilet <- nn_ps_read_data %>% 
  filter(hh_toilet_avail == 0)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = notoilet)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 6. Electricity available 
elect <- nn_ps_read_data$hh_electricity_avail == 1
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = elect)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

noelect <- nn_ps_read_data$hh_electricity_avail == 0
noelect <- nn_ps_read_data %>% 
  filter(hh_electricity_avail == 0)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = noelect)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



## 7. Household residence  
urban <- nn_ps_read_data$hh_urban == 1
urban <- nn_ps_read_data %>%
  filter(hh_urban == 1)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = urban)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

rural <- nn_ps_read_data$hh_urban == 0
rural <- nn_ps_read_data %>%
  filter(hh_urban == 0)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = rural)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


# 8. Economic Status 
hes <- nn_ps_read_data$economic_status == 1
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = hes)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

les <- nn_ps_read_data$economic_status == 0
les <- nn_ps_read_data %>% 
  filter(economic_status == 0)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = les)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



# 9. Religion 
hindu <- nn_ps_read_data$hh_religion2 == 1
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = hindu)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

nonhindu <- nn_ps_read_data$hh_religion2 == 0
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = nonhindu)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


# 10. Caste 
fc <- nn_ps_read_data$hh_caste2 == 1
fc <- nn_ps_read_data %>% 
  filter(hh_caste2 == 1)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = fc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

bc <- nn_ps_read_data$hh_caste2 == 2
bc <- nn_ps_read_data %>% 
  filter(hh_caste2 == 2)
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = bc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

oc <- nn_ps_read_data$hh_caste2 == 3
att <- average_treatment_effect(cf_reading, target.sample = "treated", subset = oc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

















############ CAUSAL FOREST ANALYSIS FOR MATH OUTCOMES 

### Loading data set 
load("nn_ps_math_data.Rdata")

nn_ps_math_data <- nn_ps_math_data %>% 
  select(-c(stateid, distid, hhid))

nn_ps_math_data <- nn_ps_math_data %>% 
  select(-c(stateid, distid, hhid))

## Setting input parameters 
X <- model.matrix(z_math_score & icds_benef ~ ., nn_ps_math_data)[, -1]
Y <- nn_ps_math_data$z_math_score
W <- nn_ps_math_data$icds_benef

## Running models for Y and W
Y_forest = regression_forest (X, Y)
Y_hat = predict(Y_forest)$predictions
W_forest = regression_forest (X, W)
W_hat = predict(W_forest)$predictions

## Raw Casual Forest 
cf_raw_math <- causal_forest(X, Y, W, Y.hat = Y_hat , W.hat = W_hat, num.trees = 4402)

## Variable Importance 
varimp <- variable_importance(cf_raw_math)
names(varimp) <- colnames(X)
sorted_varimp <- data.frame(sort(varimp, decreasing=TRUE))
selected_idx = which (varimp > mean(varimp))

## Causal Forest
cf_math <- causal_forest(X[,selected_idx], Y, W, Y.hat = Y_hat , W.hat = W_hat, num.trees = 4402)


## Testing Overlap Assumption
ggplot(data.frame(W.hat = cf_math$W.hat, W = factor(cf_math$W.orig))) + 
geom_histogram(aes(x = W.hat, y = stat(density), fill = W), alpha=0.3 , position ="identity") + 
geom_density(aes(x = W.hat, color = W)) + 
xlim(0,1) + labs(title = "Causal forest propensity scores for Math Scores",
caption = "The propensity scores are learned via GRF's regression forest")

## Out of bag predictions 
tau_hat_math <- predict(cf_math)$predictions
hist(tau_hat_math, main = "Histogram of out-of-bag CATE estimates for Math Scores", xlab = "Estimated CATT") 

## Omnibus test for heterogeneity 
test_calibration(cf_math)                                                                                              

## Best Linear Projection 
best_linear_proj <- best_linear_projection(cf_math, X)

stargazer(best_linear_proj, 
          type = "html", 
          out = "BLP-math.html",
          summary = FALSE, 
          title = "Table A: Best Linear Projection of CATT", 
          single.row = TRUE)


### ATT estimate for Math score

att <- average_treatment_effect(cf_math, target.sample = "treated")
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



###### HTE ACROSS COVARIATES SUBGROUP CATE 

# 1. Child's Gender 
female <- nn_ps_math_data$child_sex == 1
female <- nn_ps_math_data %>% 
  filter(child_sex == 1)

att <- average_treatment_effect(cf_math, target.sample = "treated", subset = female)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


male <- nn_ps_math_data$child_sex == 0
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = male)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 2. Child's school type

public <- nn_ps_math_data$school_type1 == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = public)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

private <- nn_ps_math_data$school_type1 == 2
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = private)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



## 3. Mother's education 
above_avg <- nn_ps_math_data$mother_educ > mean(nn_ps_math_data$mother_educ)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = above_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

below_avg <- nn_ps_math_data$mother_educ < mean(nn_ps_math_data$mother_educ)
below_avg <- nn_ps_math_data %>%
  filter(mother_educ < mean(mother_educ))

att <- average_treatment_effect(cf_math, target.sample = "treated", subset = below_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 4. Father's education  
above_avg <- nn_ps_math_data$father_educ > mean(nn_ps_math_data$father_educ)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = above_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

below_avg <- nn_ps_math_data$father_educ < mean(nn_ps_math_data$father_educ)
below_avg <- nn_ps_math_data %>%
  filter(father_educ < mean(father_educ))

att <- average_treatment_effect(cf_math, target.sample = "treated", subset = below_avg)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 5. Toilet available 
toilet <- nn_ps_math_data$hh_toilet_avail == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = toilet)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

notoilet <- nn_ps_math_data$hh_toilet_avail == 0
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = notoilet)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


## 6. Electricity available 
elect <- nn_ps_math_data$hh_electricity_avail == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = elect)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

noelect <- nn_ps_math_data$hh_electricity_avail == 0
noelect <- nn_ps_math_data %>% 
  filter(hh_electricity_avail == 0)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = noelect)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



## 7. Household residence  
urban <- nn_ps_math_data$hh_urban == 1
urban <- nn_ps_math_data %>%
  filter(hh_urban == 1)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = urban)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

rural <- nn_ps_math_data$hh_urban == 0
rural <- nn_ps_math_data %>%
  filter(hh_urban == 0)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = rural)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


# 8. Economic Status 
hes <- nn_ps_math_data$economic_status == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = hes)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

les <- nn_ps_math_data$economic_status == 0
les <- nn_ps_math_data %>% 
  filter(economic_status == 0)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = les)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]



# 9. Religion 
hindu <- nn_ps_math_data$hh_religion2 == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = hindu)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

nonhindu <- nn_ps_math_data$hh_religion2 == 0
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = nonhindu)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]


# 10. Caste 
fc <- nn_ps_math_data$hh_caste2 == 1
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = fc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

bc <- nn_ps_math_data$hh_caste2 == 2
bc <- nn_ps_math_data %>% 
  filter(hh_caste2 == 2)
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = bc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]

oc <- nn_ps_math_data$hh_caste2 == 3
att <- average_treatment_effect(cf_math, target.sample = "treated", subset = oc)
# estimate 
att[["estimate"]]
# std error
att[["std.err"]]
# t value 
att[["estimate"]]/att[["std.err"]]
## Confidence interval 
att[["estimate"]] + 1.96*att[["std.err"]]
att[["estimate"]] - 1.96*att[["std.err"]]





####################### HTE GRAPHS 


####### READING SCORES

reading_cf <- read_xlsx("Reading CF.xlsx")

ggplot(reading_cf, aes(CATT, Subgroup, colour = Group)) +
  geom_point() + 
  geom_errorbarh(aes(xmax = CATT + 1.96*SE, xmin = CATT - 1.96*SE), position = "identity",
                 height = 0.2) + theme_bw()


####### MATH SCORES

math_cf <- read_xlsx("Math CF.xlsx")

ggplot(math_cf, aes(CATT, Subgroup, colour = Group)) +
  geom_point() + 
  geom_errorbarh(aes(xmax = CATT + 1.96*SE, xmin = CATT - 1.96*SE), position = "identity",
                 height = 0.2) + theme_bw()



############### COMPARISON GRAPHS 


########## READING SCORES 

comparison_data <- read_xlsx("Comparison graph.xlsx")

ggplot(comparison_data, aes(x = Group, y = CATT)) + 
  geom_errorbar(aes(ymin = CATT - 1.96*SE, ymax = CATT + 1.96*SE, color = Subgroup), 
                position=position_dodge(0.3), width = 0.1) + 
  geom_point(aes(color = Subgroup), position = position_dodge(0.3)) +
  theme_bw() 


