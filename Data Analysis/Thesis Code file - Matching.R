rm(list = ls())

# Setting working directory
setwd("C:/Users/dell/Desktop/Master's Thesis")

# Installing libraries 
library(haven)
library(tidyverse)
library(stargazer)
library(MatchIt)
library(car)
library(sandwich)
library(readxl)
library(cobalt)


load("thesisdata1.Rdata")

load("thesisdata2.Rdata")


# Standardizing raw reading and math score 

merged3_df$'z_reading_score' <- ((merged3_df$raw_reading_score) - mean(merged3_df$raw_reading_score))/sd(merged3_df$raw_reading_score)
merged3_df$'z_math_score' <- ((merged3_df$raw_math_score) - mean(merged3_df$raw_math_score))/sd(merged3_df$raw_math_score)

# Creating separate data frames for girls and boys 
girls_df <- merged3_df %>%
  filter(child_sex == "female")

boys_df <- merged3_df %>%
  filter(child_sex == "male")

set.seed(123)



########################## MATCHING 

######### FULL SAMPLE

#### 1. NEAREST NEIGHBOUR MATCHING (MAHALANOBIS DISTANCE)


# Reading score 
nn_maha_read <- matchit(icds_benef ~ child_age_wave2 + child_sex + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = merged3_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_sex + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_maha_read_data <- match.data(nn_maha_read)

nn_maha_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid +
                         distid + hhid,
                       data = nn_maha_read_data, weights = weights)


summary(nn_maha_read)


v <- data.frame(old = c("child_age_wave2", "child_sex_female", "child_family_type_joint",
          "mother_age", "mother_educ", "mother_health_good", "mother_surviving_children",
          "father_educ", "school_type_public", "school_type_private", "school_type_other",
           "teacher_regular", "teacher_good", "midday_meal", "hh_water_avail", 
           "hh_toilet_avail", "hh_electricity_avail", "hh_meals_perday", "hh_urban", 
          "hh_poor", "log_hhexpend", "hh_religion_hindu", "hh_religion_muslim", 
          "hh_religion_christian", "hh_religion_other", "hh_caste_forward caste", 
          "hh_caste_obc", "hh_caste_dalit", "hh_caste_other"), 
          new = c("Child’s age", "Child’s sex:female", "Child’s family type:joint", 
          "Mother’s age", "Mother’s education", "Mother’s health:good", 
           "Number of surviving children", "Father’s education", "Child’s school type:public", 
          "Child’s school type:private", 
          "Child’s school type:other", "Child's teacher is regular", "Child's teacher is good", 
         "Child receives mid-day meals", "Water available in household", "Toilet available in household", 
         "Electricity available in household", "Number of meals per day",
          "Household's residence:urban", "Household is below poverty line", 
          "Household's annual consumption expenditure", "Household's religion:Hindu", 
         "Household's religion:Muslim", "Household's religion:Christian", 
         "Household's religion:Other", 'Household’s caste:Forward caste',"Household’s caste:OBC", 
         "Household’s caste:Dalit", "Household’s caste:Other"))


# Covariate Balance Plot 
love.plot(nn_maha_read, var.order = "unadjusted", var.names = v,  drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Test for Common Support 
bal.plot(nn_maha_read, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))


# Math Score 

nn_maha_math <- matchit(icds_benef ~ child_age_wave2 + child_sex + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = merged3_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_sex + child_family_type +
                        mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        estimand = "ATT", 
                        m.order = "random",
                        ratio = 1, 
                        replace = F)


nn_maha_math_data <- match.data(nn_maha_math)

nn_maha_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                         mother_age + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + school_type + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                       data = nn_maha_math_data, weights = weights)

summary(nn_maha_math, improvement = TRUE)

# Covariate Balance Plot 
love.plot(nn_maha_math, var.order = "unadjusted", var.names = v,  drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Test for Common Support 
bal.plot(nn_maha_math, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))


# Calculating clustered standard errors for both models

clse_nn_maha_r <- vcovCL(nn_maha_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_maha_m <- vcovCL(nn_maha_math_att, cluster = ~ distid + hhid + subclass) 

nn_maha_read_se <- sqrt(diag(clse_nn_maha_r))
nn_maha_math_se <- sqrt(diag(clse_nn_maha_m))




#### 2. NEAREST NEIGHBOUR MATCHING (PROPENSITY SCORE)


# Reading score 
nn_ps_read <- matchit(icds_benef ~ child_age_wave2 + child_sex + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                       data = merged3_df,
                       method = "nearest",
                       distance = "glm",
                      discard = "control",
                      m.order = "random",
                      estimand = "ATT", 
                       ratio = 1, 
                       replace = F)

nn_ps_read_data <- match.data(nn_ps_read)

nn_ps_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                        mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
                      data = nn_ps_read_data, weights = weights)

summary(nn_ps_read, improvement = TRUE)

# Covariate Balance Plot 
love.plot(nn_ps_read, var.order = "unadjusted", var.names = v,  drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Test for Common Support 
bal.plot(nn_ps_read, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))


# Math score 

nn_ps_math <- matchit(icds_benef ~ child_age_wave2 + child_sex + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                       data = merged3_df,
                       method = "nearest",
                       distance = "glm",
                      discard = "control",
                      m.order = "random",
                       estimand = "ATT", 
                       ratio = 1, 
                       replace = F)

nn_ps_math_data <- match.data(nn_ps_math)

nn_ps_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                        mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                      data = nn_ps_math_data, weights = weights)

summary(nn_ps_math, improvement = TRUE)

# Covariate Balance Plot 
love.plot(nn_ps_math, var.order = "unadjusted", var.names = v,  drop.distance = TRUE, 
          abs = FALSE, binary = "std", thresholds = 0.1, grid = TRUE, 
          sample.names = c("Unmatched", "Matched"), limits = c(-1, 1))

# Test for Common Support 
bal.plot(nn_ps_math, var.name = "distance", which = "both", type = "histogram", 
         mirror = TRUE, sample.names = c("Unmatched", "Matched"))

# Calculating clustered standard errors for both models

clse_nn_ps_r <- vcovCL(nn_ps_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_ps_m <- vcovCL(nn_ps_math_att, cluster = ~ distid + hhid + subclass) 

nn_ps_read_se <- sqrt(diag(clse_nn_ps_r))
nn_ps_math_se <- sqrt(diag(clse_nn_ps_m))


matching_models <- list(fe_mod1, 
                        fe_mod2, 
                        nn_maha_read_att, 
                        nn_maha_math_att, 
                        nn_ps_read_att,
                        nn_ps_math_att)

matching_se <- list(fe_mod1_se, fe_mod2_se, nn_maha_read_se, nn_maha_math_se, nn_ps_read_se, nn_ps_math_se)


################## CREATING A REGRESSION TABLE 

stargazer(matching_models,
          se = matching_se, 
          type = "html", 
          dep.var.labels = c("Pre-matching", "Pre-matching", "Mahalanobis", "Mahalanobis", "Propensity Score", "Propensity Score"), 
          column.labels = c("Reading Score", "Math Score", "Reading Score", "Math Score", "Reading Score", "Math Score"), 
          column.separate = c(1,1,1,1, 1, 1), 
          dep.var.caption = "",
          column.sep.width = "3pt",
          title = "Table 3: Pre and Post-matching OLS fixed effect estimates (Full Sample): Nearest Neighbour Matching with Propensity Score and Mahalanobis Distance", 
          out = "matching-ps-maha.html",
          omit = c("Constant", "wtew"),
          single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "f"))








######### GIRLS

#### 1. NEAREST NEIGHBOUR MATCHING (MAHALANOBIS DISTANCE)

# Reading score 
nn_maha_read <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = girls_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_maha_read_data <- match.data(nn_maha_read)

nn_maha_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid +
                         distid + hhid,
                       data = nn_maha_read_data, weights = weights)



# Math Score 

nn_maha_math <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = girls_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_maha_math_data <- match.data(nn_maha_math)

nn_maha_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                       data = nn_maha_math_data, weights = weights)

# Calculating clustered standard errors for both models

clse_nn_maha_r <- vcovCL(nn_maha_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_maha_m <- vcovCL(nn_maha_math_att, cluster = ~ distid + hhid + subclass) 

nn_maha_read_se <- sqrt(diag(clse_nn_maha_r))
nn_maha_math_se <- sqrt(diag(clse_nn_maha_m))



#### 2. NEAREST NEIGHBOUR MATCHING (PROPENSITY SCORE)

# Reading score 
nn_ps_read <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = girls_df,
                        method = "nearest",
                        distance = "glm",
                      discard = "control",
                      m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_ps_read_data <- match.data(nn_ps_read)

nn_ps_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid +
                        distid + hhid,
                       data = nn_ps_read_data, weights = weights)


stargazer(nn_ps_read_att, 
          type = "html", 
          out = "maha_att.html")

# Math Score 

nn_ps_math <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = girls_df,
                        method = "nearest",
                        distance = "glm",
                      discard = "control",
                      m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_ps_math_data <- match.data(nn_ps_math)

nn_ps_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                       data = nn_ps_math_data, weights = weights)

# Calculating clustered standard errors for both models

clse_nn_ps_r <- vcovCL(nn_ps_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_ps_m <- vcovCL(nn_ps_math_att, cluster = ~ distid + hhid + subclass) 

nn_ps_read_se <- sqrt(diag(clse_nn_ps_r))
nn_ps_math_se <- sqrt(diag(clse_nn_ps_m))



matching_models <- list(fe_mod3, 
                        fe_mod4,
                        nn_maha_read_att, 
                        nn_maha_math_att, 
                        nn_ps_read_att,
                        nn_ps_math_att)

matching_se <- list(fe_mod3_se, fe_mod4_se, nn_maha_read_se, nn_maha_math_se, nn_ps_read_se, nn_ps_math_se)


# Post Matching results - Girls  
stargazer(matching_models,
          se = matching_se, 
          type = "html", 
          dep.var.labels = c("Pre-matching", "Pre-matching", "Mahalanobis", "Mahalanobis", "Propensity Score", "Propensity Score"), 
          column.labels = c("Reading Score", "Math Score", "Reading Score", "Math Score", "Reading Score", "Math Score"), 
          column.separate = c(1,1,1,1, 1, 1), 
          dep.var.caption = "",
          column.sep.width = "3pt",
          title = "Table 3: Pre and Post-matching OLS fixed effect estimates (Sample of Girls): Nearest Neighbour Matching with Propensity Score and Mahalanobis Distance", 
          out = "matching-ps-maha-girls.html",
          omit = c("Constant", "wtew"),
          single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "f"))







######### BOYS

#### 1. NEAREST NEIGHBOUR MATCHING (MAHALANOBIS DISTANCE)


# Reading score 
nn_maha_read <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = boys_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_maha_read_data <- match.data(nn_maha_read)

nn_maha_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                       data = nn_maha_read_data, weights = weights)


# Math Score 

nn_maha_math <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                        data = boys_df,
                        method = "nearest",
                        distance = "glm",
                        mahvars =  ~ child_age_wave2 + child_family_type +
                          mother_age  + mother_educ + mother_health + mother_surviving_children + 
                          father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                          hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                          hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                        discard = "control", 
                        m.order = "random",
                        estimand = "ATT", 
                        ratio = 1, 
                        replace = F)

nn_maha_math_data <- match.data(nn_maha_math)

nn_maha_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
                         mother_age  + mother_educ + mother_health + mother_surviving_children + 
                         father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                         hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                         hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                       data = nn_maha_math_data, weights = weights)

# Calculating clustered standard errors for both models

clse_nn_maha_r <- vcovCL(nn_maha_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_maha_m <- vcovCL(nn_maha_math_att, cluster = ~ distid + hhid + subclass) 

nn_maha_read_se <- sqrt(diag(clse_nn_maha_r))
nn_maha_math_se <- sqrt(diag(clse_nn_maha_m))



#### 2. NEAREST NEIGHBOUR MATCHING (PROPENSITY SCORE)

# Reading score 
nn_ps_read <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                        mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                      data = boys_df,
                      method = "nearest",
                      distance = "glm",
                      discard = "control",
                      m.order = "random",
                      estimand = "ATT", 
                      ratio = 1, 
                      replace = F)

nn_ps_read_data <- match.data(nn_ps_read)

nn_ps_read_att <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                       mother_age  + mother_educ + mother_health + mother_surviving_children + 
                       father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                       hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                       hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                     data = nn_ps_read_data, weights = weights)


# Math Score 

nn_ps_math <- matchit(icds_benef ~ child_age_wave2 + child_family_type +
                        mother_age  + mother_educ + mother_health + mother_surviving_children + 
                        father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                        hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                        hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste, 
                      data = boys_df,
                      method = "nearest",
                      distance = "glm",
                      discard = "control",
                      m.order = "random",
                      estimand = "ATT", 
                      ratio = 1, 
                      replace = F)

nn_ps_math_data <- match.data(nn_ps_math)

nn_ps_math_att <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
                       mother_age  + mother_educ + mother_health + mother_surviving_children + 
                       father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                       hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                       hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid,
                     data = nn_ps_math_data, weights = weights)

# Calculating clustered standard errors for both models

clse_nn_ps_r <- vcovCL(nn_ps_read_att, cluster = ~ distid + hhid + subclass)
clse_nn_ps_m <- vcovCL(nn_ps_math_att, cluster = ~ distid + hhid + subclass) 

nn_ps_read_se <- sqrt(diag(clse_nn_ps_r))
nn_ps_math_se <- sqrt(diag(clse_nn_ps_m))



matching_models <- list(fe_mod5, 
                        fe_mod6,
                        nn_maha_read_att, 
                        nn_maha_math_att, 
                        nn_ps_read_att,
                        nn_ps_math_att)

matching_se <- list(fe_mod5_se, fe_mod6_se, nn_maha_read_se, nn_maha_math_se, nn_ps_read_se, nn_ps_math_se)


# Post Matching results - Boys  
stargazer(matching_models,
          se = matching_se, 
          type = "html", 
          dep.var.labels = c("Pre-matching", "Pre-matching", "Mahalanobis", "Mahalanobis", "Propensity Score", "Propensity Score"), 
          column.labels = c("Reading Score", "Math Score", "Reading Score", "Math Score", "Reading Score", "Math Score"), 
          column.separate = c(1,1,1,1, 1, 1), 
          dep.var.caption = "",
          column.sep.width = "3pt",
          title = "Table 3: Pre and Post-matching OLS fixed effect estimates (Sample of Boys): Nearest Neighbour Matching with Propensity Score and Mahalanobis Distance", 
          out = "matching-ps-maha-boys.html",
          omit = c("Constant", "wtew"),
          single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "f"))







############# BALANCE TABLE 

balance_table <- bal.tab(icds_benef ~ child_age_wave2 + child_sex + child_family_type +
                           mother_age  + mother_educ + mother_health + mother_surviving_children + 
                           father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                           hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                           hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste,
                         data = merged3_df, 
                         un = TRUE, 
                         binary = "std", 
                         weights = list(maha = nn_maha_read, ps = nn_ps_read))

balance_table <- balance_table[["Balance"]]
balance_table <- as.data.frame(balance_table)

balance_table <- balance_table %>% 
  select(Diff.Un, Diff.maha, Diff.ps)

balance_table <- balance_table %>%
  rename('Pre-Matching' = Diff.Un, 'Mahalanobis Matching' = Diff.maha, 'Propensity Score Matching' = Diff.ps)


  
stargazer(balance_table, 
          title = "Assessing Balance Pre and Post-matching using Covariate Standardized Mean Differences (SMD)",
          type = "html", 
          out = "balance-read.html", 
          summary = FALSE)


