"
PROJECT NAME: GEAR PROGRAM IN RMG SECTOR OF BANGLADESH
SUPERVISOR: DR. MAHREEN KHAN 

SUMMARY: OLS FIXED EFFECTS AND LOGISTIC REGRESSION ANALYSIS USING OTHER OUTCOME
         VARIABLES UNDER WORKING CONDITIONS AND MANAGEMENT PRACTICES 

R VERSION: 4.3.2 

THIS R SCRIPT HAS THE FOLLOWING SECTIONS:
"


### Clearing the environment 
rm(list=ls())

### Setting up working directory to store the output files 
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path before running the command
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/RMG Study/Output/WC and MP outcomes/Graphs and plots")

### Loading libraries (if package not already installed then use install.packages() to install the library first)
library(haven)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(dplyr)
library(patchwork)




############################### COMPARISON GRAPHS ##########################################


############################### WORKING CONDITIONS OTHER OUTCOMES

########### 1. Without interaction models 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 1)

outcome_colors <- c("There are opportunities for career advancement in the factory" = "blue", 
                    "Operators can make their voice heard if they have a concern" = "blue", 
                    "Operator was discouraged to take leaves in the last month" = "red", 
                    "Supervisor supportive towards family related issues" = "blue",
                    "Operator felt ill-treated at work due to gender" = "blue",
                    "Operator felt safe under Male supervisor" = "red", 
                    "Operator felt safe under Female supervisor" = "red", 
                    "Operators know what is expected of them at work" = "blue", 
                    "Operators get to use my best skills and talent at work" = "blue", 
                    "Supervisors treat operators fairly" = "red", 
                    "Production line management cares about well-being of employees" = "blue", 
                    "Line managers help operators deal with stressful situations at work" = "blue", 
                    "Operators work schedule is predictable" = "blue", 
                    "Operators have sufficient rest/break time" = "blue", 
                    "Operators feel listened to" = "blue", 
                    "Operators feel they are treated with respect" = "blue", 
                    "Operators feel they have career opportunities" = "blue", 
                    "Operators feel appreciated and recognized" = "blue", 
                    "Operators feel they have job training and support" = "blue")

plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_1.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for all binary outcomes of Working Conditions with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models without the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                       plot.caption = element_text(hjust = 0.9, size = 9))) + 
                       theme(legend.position = "none")
dev.off()


########### 2. With interaction models 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 2)

outcome_colors <- c("There are opportunities for career advancement in the factory" = "red", 
                    "Operators can make their voice heard if they have a concern" = "red", 
                    "Operator was discouraged to take leaves in the last month" = "blue", 
                    "Supervisor supportive towards family related issues" = "blue",
                    "Operator felt ill-treated at work due to gender" = "blue",
                    "Operator felt safe under Male supervisor" = "blue", 
                    "Operator felt safe under Female supervisor" = "blue", 
                    "Operators know what is expected of them at work" = "blue", 
                    "Operators get to use my best skills and talent at work" = "blue", 
                    "Supervisors treat operators fairly" = "blue", 
                    "Production line management cares about well-being of employees" = "blue", 
                    "Line managers help operators deal with stressful situations at work" = "blue", 
                    "Operators work schedule is predictable" = "blue", 
                    "Operators have sufficient rest/break time" = "blue", 
                    "Operators feel listened to" = "blue", 
                    "Operators feel they are treated with respect" = "blue", 
                    "Operators feel they have career opportunities" = "blue", 
                    "Operators feel appreciated and recognized" = "blue", 
                    "Operators feel they have job training and support" = "blue")

plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_2.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for all binary outcomes of Working Conditions with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models with the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                                     plot.caption = element_text(hjust = 0.9, size = 9))) + 
  theme(legend.position = "none")
dev.off()


################## SEPARATE COMPOENENTS OF WORKING CONDITIONS INDEX 

##### Without Interaction 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 3)

outcome_colors <- c("Supervisor gives extra support to less skilled operators" = "red", 
                    "Supervisor uses praise to motivate operators" = "red", 
                    "Supervisor uses less shouting or abusive language to motivate operators" = "red", 
                    "Supervisor involve sewing operators in solving problems on the line" = "red",
                    "Supervisor demonstrates tasks in front of operators" = "red",
                    "Supervisor sews herself if an operator is absent or needs to go to the washroom" = "red", 
                    "Supervisor communicates the skills you need to acquire to be promoted" = "red", 
                    "Supervisor helps gain the skills you need to be promoted" = "red")


plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_3.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for separate components of Working Conditions Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models without the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                                     plot.caption = element_text(hjust = 0.9, size = 9))) + 
  theme(legend.position = "none")
dev.off()


#### With Interaction 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 4)

outcome_colors <- c("Supervisor gives extra support to less skilled operators" = "red", 
                    "Supervisor uses praise to motivate operators" = "red", 
                    "Supervisor uses less shouting or abusive language to motivate operators" = "red", 
                    "Supervisor involve sewing operators in solving problems on the line" = "red",
                    "Supervisor demonstrates tasks in front of operators" = "red",
                    "Supervisor sews herself if an operator is absent or needs to go to the washroom" = "red", 
                    "Supervisor communicates the skills you need to acquire to be promoted" = "blue", 
                    "Supervisor helps gain the skills you need to be promoted" = "red")


plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_4.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for separate components of Working Conditions Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models with the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.8, vjust = 0.8, size = 12, face = "bold"), 
                                     plot.caption = element_text(hjust = 0.9, size = 9))) + 
  theme(legend.position = "none")
dev.off()


################## SEPARATE COMPOENENTS OF MANAGEMENT PRACTICES INDEX 

##### Without Interaction 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 5)

outcome_colors <- c("My supervisor is more confident" = "red", 
                    "My supervisor is better at remaining calm in stressful situations" = "red", 
                    "My supervisor is better at motivating operators" = "red", 
                    "My supervisor is better at correcting mistakes and ensuring product quality" = "red",
                    "My supervisor is better at helping operators if they have problems at their workstations" = "blue",
                    "My supervisor is better at encouraging operators to take leadership positions" = "blue", 
                    "My supervisor is better at helping operators improve their skills" = "blue", 
                    "My supervisor knows better which machines are appropriate for which tasks" = "blue", 
                    "My supervisor is better at meeting production targets" = "blue")

plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_5.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for separate components of Management Practices Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models without the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 5-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.9, vjust = 0.8, size = 12, face = "bold"), 
                                     plot.caption = element_text(hjust = 0.9, size = 9))) + 
  theme(legend.position = "none")
dev.off()



#### With Interaction 
point_estimates <- read_excel("point_estimates.xlsx", sheet = 6)

outcome_colors <- c("My supervisor is more confident" = "blue", 
                    "My supervisor is better at remaining calm in stressful situations" = "blue", 
                    "My supervisor is better at motivating operators" = "blue", 
                    "My supervisor is better at correcting mistakes and ensuring product quality" = "red",
                    "My supervisor is better at helping operators if they have problems at their workstations" = "blue",
                    "My supervisor is better at encouraging operators to take leadership positions" = "blue", 
                    "My supervisor is better at helping operators improve their skills" = "blue", 
                    "My supervisor knows better which machines are appropriate for which tasks" = "blue", 
                    "My supervisor is better at meeting production targets" = "blue")


plot <- ggplot(point_estimates, aes(x = Estimate, y = Outcome)) + 
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

jpeg("graph_6.jpeg", pointsize = 4, width=2000, height=960, res=150)
plot + plot_annotation(title = "Treatment Effects for separate components of Management Practices Index with 95% Confidence Intervals", 
                       caption = "Note: The estimates are from the logistic regression models with the treatment and supervisor work experience interaction. 
                                Estimates in red are statistically significant. For all outcomes, the values were orginally based on a 4-point Likert Scale. However, for analysis, they were converted into binary outcomes, where Xij = 5 was coded 1 and Xij < 5 was coded 0.",
                       theme = theme(plot.title = element_text(hjust = 0.9, vjust = 0.8, size = 12, face = "bold"), 
                                     plot.caption = element_text(hjust = 0.9, size = 9))) + 
  theme(legend.position = "none")
dev.off()