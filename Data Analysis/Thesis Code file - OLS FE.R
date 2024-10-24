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


summary(mod <- lm(z_reading_score ~ icds_benef + stateid, data = merged3_df))


######### OLS FIXED EFFECTS REGRESSION (BEFORE MATCHING)

#### FULL SAMPLE 

# Reading score
fe_mod1 <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                mother_age  + mother_educ + mother_health + mother_surviving_children + 
                father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
              data = merged3_df)
summary(fe_mod1)
vif(fe_mod1)

# Math score
fe_mod2 <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_sex + child_family_type +
                mother_age  + mother_educ + mother_health + mother_surviving_children + 
                father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
              data = merged3_df)
summary(fe_mod2)
vif(fe_mod2)

# Calculating clustered standard errors for both models 

clse_mod1 <- vcovCL(fe_mod1, cluster = ~ distid + hhid)
clse_mod2 <- vcovCL(fe_mod2, cluster = ~ distid + hhid) 

fe_mod1_se <- sqrt(diag(clse_mod1))
fe_mod2_se <- sqrt(diag(clse_mod2))


# TABLE 1 Appendix - OLS fixed effects regression (Full Sample)

stargazer(fe_mod1, 
          fe_mod2,
          se = list(fe_mod1_se, fe_mod2_se),
          type = "html", 
          dep.var.labels.include = FALSE, 
          column.labels = c("Reading Score", "Math Score"), 
          column.separate = c(1,1), 
          dep.var.caption = "",
          column.sep.width = "3pt",
          title = "Table A1: OLS Fixed Effects Regression - Full Sample", 
          out = "femod-full.html",
          omit = c("Constant", "wtew"),
          single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "f"))



#### ONLY GIRLS  

# Reading score
fe_mod3 <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                mother_age  + mother_educ + mother_health + mother_surviving_children + 
                father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
              data = girls_df)
summary(fe_mod3)

# Math score 
fe_mod4 <- lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
                mother_age  + mother_educ + mother_health + mother_surviving_children + 
                father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
              data = girls_df)
summary(fe_mod4)

# Calculating clustered standard errors for both models 

clse_mod3 <- vcovCL(fe_mod3, cluster = ~ distid + hhid)
clse_mod4 <- vcovCL(fe_mod4, cluster = ~ distid + hhid) 

fe_mod3_se <- sqrt(diag(clse_mod3))
fe_mod4_se <- sqrt(diag(clse_mod4))




#### ONLY BOYS  

# Reading score
fe_mod5 <- lm(z_reading_score ~ icds_benef + child_age_wave2 + child_family_type +
                mother_age  + mother_educ + mother_health + mother_surviving_children + 
                father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
                hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
                hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
              data = boys_df)
summary(fe_mod5)

# Math score 
fe_mod6 <-lm(z_math_score ~ icds_benef + child_age_wave2 + child_family_type +
               mother_age  + mother_educ + mother_health + mother_surviving_children + 
               father_educ + school_type + teacher_regular + teacher_good + midday_meal + 
               hh_water_avail + hh_toilet_avail + hh_electricity_avail + hh_meals_perday + 
               hh_urban +  hh_poor + log_hhexpend + hh_religion + hh_caste + wtew + stateid, 
             data = boys_df)
summary(fe_mod6)

# Calculating clustered standard errors for both models

clse_mod5 <- vcovCL(fe_mod5, cluster = ~ distid + hhid)
clse_mod6 <- vcovCL(fe_mod6, cluster = ~ distid + hhid) 

fe_mod5_se <- sqrt(diag(clse_mod5))
fe_mod6_se <- sqrt(diag(clse_mod6))

fe_models <- list(fe_mod3, fe_mod4, fe_mod5, fe_mod6)



# TABLE 2 Appendix - OLS fixed effects regression (Girls and Boys)

stargazer(fe_mod3, 
          fe_mod4, 
          fe_mod5, 
          fe_mod6,
          se = list(fe_mod3_se, fe_mod4_se, fe_mod5_se, fe_mod6_se),
          type = "html", 
          dep.var.labels = c("Girls", "Girls", "Boys", "Boys"), 
          column.labels = c("Reading Score", "Math Score", "Reading Score", "Math Score"), 
          column.separate = c(1,1,1,1), 
          dep.var.caption = "",
          column.sep.width = "3pt",
          title = "Table A2: OLS Fixed Effects Regression - Girls and Boys", 
          out = "femod-g&b.html",
          omit = c("Constant", "wtew"),
          single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "f"))


# TABLE 3 Appendix - Variance Inflation Factors from OLS FE estimates 

vif_data <- read_excel("vifdata.xlsx")
vif_df <- as.data.frame(vif_data)

vif_df <- vif_df %>%
  rename(Variable = Covariates, DF = df)

stargazer(vif_df, 
          title = " Generalised variance inflation factors from OLS fixed effect estimates - Full Sample",
          type = "html", 
          out = "vif.html", 
          summary = FALSE)




