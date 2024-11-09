## R Version: 4.3.2 

## Title: Data Visualization - Plotting coefficient estimates with confidence intervals

## Date Created: June 27, 2024
## Date Updated: July 20, 2024

## This R script has the R code for plotting logistic regression coefficients estimates with their confidence intervals. 
## There were two models for every binary outcome: one with an interaction term in the regression model, and one without the interaction term. 
## Thus, for all outcomes, there are two plots corresponding to the different regression models. 
## Coefficient estimates and standard errors from logistic regression are expressed in odds ratio. 

## Brief Project Description: 
# The aim of the project is to evaluate the impact of a  training program rolled out by IFC and ILO in Bangladesh
# on working conditions and management practices in factory, along with worker productivity. The program trained 
# only female workers in a factory, who could then be promoted to supervisory roles.  

## Brief description of data: 
# The data used in the analysis comes from surveying the line operators and supervisors in the factory. 
# For the analysis, line operator and supervisor data were merged.   

## Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  

### Clearing R environment 
rm(list=ls())

### Loading libraries (if package not already installed then use install.packages() to install the library first)
library(haven)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(dplyr)
library(patchwork)
library(ggpubr)
library(readxl)

### Setting working directory
setwd("/Users/Desktop/Graphs and plots")

########################################################################################################
#################### PLOTTING REGRESSION COEFFICIENT ESTIMATES WITH CONFIDENCE INTERVALS ###############
########################################################################################################

############################ 1. SEPARATE COMPONENTS OF WORKING CONDITIONS INDEX 

############## 1.1 Without Interaction Logistic Regression Estimates

## Importing regression estimates data 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 3)

## Adding colors for regression estimates of all outcomes: blue if significant and blue if not significant 
outcome_colors <- c("Supervisor gives extra support to less skilled operators" = "red", 
                    "Supervisor uses praise to motivate operators" = "red", 
                    "Supervisor uses less shouting or abusive language to motivate operators" = "red", 
                    "Supervisor involve sewing operators in solving problems on the line" = "red",
                    "Supervisor demonstrates tasks in front of operators" = "red",
                    "Supervisor sews herself if an operator is absent or needs to go to the washroom" = "red", 
                    "Supervisor communicates the skills you need to acquire to be promoted" = "red", 
                    "Supervisor helps gain the skills you need to be promoted" = "red")

## Creating the plot using ggplot2 package
wc_wi_plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = Outcome), 
                position=position_dodge(0.3), width = 0.1) + 
  scale_colour_manual(values = outcome_colors) +
  geom_point(aes(color = Outcome), position = position_dodge(0.3)) + 
  scale_colour_manual(values = outcome_colors) +
  geom_vline(xintercept=1, linetype="dashed", color = "black") +
  labs(x = "Treatment Effect Estimates", y = "Outcomes") +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme_grey()

## Adding title, caption, and theme to the plot using patchwork package
wc_wi_plot <- wc_wi_plot + plot_annotation(title = "Treatment Effects for separate components of Working Conditions Index with 95% Confidence Intervals", 
                                 caption = "Note: The estimates are from the logistic regression models without the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                                 theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                                plot.caption = element_text(hjust = 0.9, size = 9))) + theme(legend.position = "none")


############## 1.2 With Interaction Logistic Regression Estimates

## Importing regression estimates data 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 4)

## Adding colors for regression estimates of all outcomes: blue if significant and blue if not significant 
outcome_colors <- c("Supervisor gives extra support to less skilled operators" = "red", 
                    "Supervisor uses praise to motivate operators" = "red", 
                    "Supervisor uses less shouting or abusive language to motivate operators" = "red", 
                    "Supervisor involve sewing operators in solving problems on the line" = "red",
                    "Supervisor demonstrates tasks in front of operators" = "red",
                    "Supervisor sews herself if an operator is absent or needs to go to the washroom" = "red", 
                    "Supervisor communicates the skills you need to acquire to be promoted" = "blue", 
                    "Supervisor helps gain the skills you need to be promoted" = "red")

## Creating the plot using ggplot2 package
wc_i_plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = Outcome), 
                position=position_dodge(0.3), width = 0.1) + 
  scale_colour_manual(values = outcome_colors) +
  geom_point(aes(color = Outcome), position = position_dodge(0.3)) + 
  scale_colour_manual(values = outcome_colors) +
  geom_vline(xintercept=1, linetype="dashed", color = "black") +
  labs(x = "Treatment Effect Estimates", y = "Outcomes") +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme_grey()

## Adding title, caption, and theme to the plot using patchwork package
wc_i_plot <- wc_i_plot + plot_annotation(title = "Treatment Effects for separate components of Working Conditions Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models with the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                      plot.caption = element_text(hjust = 0.9, size = 9))) + theme(legend.position = "none")

## Combining both the plots using ggpubr package and saving as a jpeg image
jpeg("wc_logit_plot.jpeg", pointsize = 4, width=2000, height=1500, res=150)
ggarrange(wc_wi_plot, wc_i_plot, nrow = 2, ncol = 1)
dev.off()


############################ 2. SEPARATE COMPONENTS OF MANAGEMENT PRACTICES INDEX 

############## 2.1 Without Interaction Logistic Regression Estimates

## Importing regression estimates data 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 5)

## Adding colors for regression estimates of all outcomes: blue if significant and blue if not significant 
outcome_colors <- c("My supervisor is more confident" = "red", 
                    "My supervisor is better at remaining calm in stressful situations" = "red", 
                    "My supervisor is better at motivating operators" = "red", 
                    "My supervisor is better at correcting mistakes and ensuring product quality" = "red",
                    "My supervisor is better at helping operators if they have problems at their workstations" = "blue",
                    "My supervisor is better at encouraging operators to take leadership positions" = "blue", 
                    "My supervisor is better at helping operators improve their skills" = "blue", 
                    "My supervisor knows better which machines are appropriate for which tasks" = "blue", 
                    "My supervisor is better at meeting production targets" = "blue")

## Creating the plot using ggplot2 package
mp_wi_plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = Outcome), 
                position=position_dodge(0.3), width = 0.1) + 
  scale_colour_manual(values = outcome_colors) +
  geom_point(aes(color = Outcome), position = position_dodge(0.3)) + 
  scale_colour_manual(values = outcome_colors) +
  geom_vline(xintercept=1, linetype="dashed", color = "black") +
  labs(x = "Treatment Effect Estimates", y = "Outcomes") +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme_grey()

## Adding title, caption, and theme to the plot using patchwork package
mp_wi_plot <- mp_wi_plot + plot_annotation(title = "Treatment Effects for separate components of Management Practices Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models without the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.9, vjust = 0.8, size = 12, face = "bold"), 
                        plot.caption = element_text(hjust = 0.9, size = 9))) + theme(legend.position = "none")


############## 2.2 With Interaction Logistic Regression Estimates

## Importing regression estimates data 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 6)

## Adding colors for regression estimates of all outcomes: blue if significant and blue if not significant 
outcome_colors <- c("My supervisor is more confident" = "blue", 
                    "My supervisor is better at remaining calm in stressful situations" = "blue", 
                    "My supervisor is better at motivating operators" = "blue", 
                    "My supervisor is better at correcting mistakes and ensuring product quality" = "red",
                    "My supervisor is better at helping operators if they have problems at their workstations" = "blue",
                    "My supervisor is better at encouraging operators to take leadership positions" = "blue", 
                    "My supervisor is better at helping operators improve their skills" = "blue", 
                    "My supervisor knows better which machines are appropriate for which tasks" = "blue", 
                    "My supervisor is better at meeting production targets" = "blue")

## Creating the plot using ggplot2 package
mp_i_plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = Outcome), 
                position=position_dodge(0.3), width = 0.1) + 
  scale_colour_manual(values = outcome_colors) +
  geom_point(aes(color = Outcome), position = position_dodge(0.3)) + 
  scale_colour_manual(values = outcome_colors) +
  geom_vline(xintercept=1, linetype="dashed", color = "black") +
  labs(x = "Treatment Effect Estimates", y = "Outcomes") +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme_grey()

## Adding title, caption, and theme to the plot using patchwork package
mp_i_plot <- mp_i_plot + plot_annotation(title = "Treatment Effects for separate components of Management Practices Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models with the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 4-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.9, vjust = 0.8, size = 12, face = "bold"), 
                       plot.caption = element_text(hjust = 0.9, size = 9))) + theme(legend.position = "none")

## Combining both the plots using ggpubr package and saving as a jpeg image
jpeg("mp_logit_plot.jpeg", pointsize = 4, width=2000, height=1500, res=150)
ggarrange(mp_wi_plot, mp_i_plot, nrow = 2, ncol = 1)
dev.off()

################################### END OF SCRIPT #################################################