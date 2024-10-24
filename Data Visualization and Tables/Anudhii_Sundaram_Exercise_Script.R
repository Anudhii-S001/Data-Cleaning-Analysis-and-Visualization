"
PROJECT NAME: EC2 EMPIRICAL EXERCISE FOR EDU-ANALYTICS WORLD BANK

R Version: 4.3.2 

THIS R SCRIPT HAS THE FOLLOWING SECTIONS: 

SECTION 1: CODE FOR QUESTION 2
SECTION 2: CODE FOR QUESTION 3
SECTION 3: ANSWER FOR QUESTION 4
"

## Clearing the environment 
rm(list=ls())

## Setting up working directory to store the output files 
# Note: Change the working directory path based on where the output files should be stored 
# Note: Add double inverted commas before and after the file path before running the command
setwd("/Users/anudhiisundaram/Desktop/World Bank/Empirical_Exercise_Output")

## Loading libraries 
# Note: If package not already installed then use install.packages() to install the library first
library(readr)
library(tidyverse)
library(stargazer)
library(ltm)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(wbstats)
library(htmltools)


###############################################################################################
######################## SECTION 1: CODE FOR QUESTION 2 #######################################

## Part (a) Importing data for the Children under 5 interview 

# Change the file path based on where the csv file is stored
zimbabwe_children_data <- as.data.frame(read_csv("~/Desktop/World Bank/EC2_empirical_exercise/01_rawdata/Zimbabwe_children_under5_interview.csv"))


## Part (b) Recoding survey response for variables EC6 to EC15 such that Yes=1, No=0, and DK=0

zimbabwe_children_data <- zimbabwe_children_data %>%
  mutate(across(starts_with("EC"), ~ recode(.x, "1" = 1, "2" = 0, "8" = 0, "9" = 0)))


## Part (c) Creating a table of summary statistics by child's age in years

# Creating separate data frames by child's age in years  
child_age_3 <- zimbabwe_children_data %>%
  filter(child_age_years==3)

child_age_4 <- zimbabwe_children_data %>%
  filter(child_age_years==4)

# Creating a summary statistics table 
stargazer(child_age_3, child_age_4, 
          type = "html",
          title = c("Table 1a: Summary statistics for 3-year-old children in the sample", 
                    "Table 1b: Summary statistics for 4-year-old children in the sample"),  
          digits = 2, 
          covariate.labels = c("Can the child identify or name at least ten letters of the alphabet?", 
                               "Can the child read at least four simple, popular words?", 
                               "Does the child know the name and recognize the symbol of all numbers from 1 to 10?", 
                               "Can the child pick up a small object with two fingers, like a stick or a rock from the ground?", 
                               "Is the child sometimes too sick to play?", 
                               "Does the child follow simple directions on how to do something correctly?", 
                               "When given something to do, is the child able to do it independently?", 
                               "Does the child get along well with other children?", 
                               "Does the child kick, bite, or hit other children or adults?", 
                               "Does the child get distracted easily?"), 
          omit = c("interview_date", "child_age_years", "child_birthday"),
          out = "table1.html")


## Part (d) Calculating the Overall Index using variables EC6 to EC15

zimbabwe_children_data$'Overall' <- rowMeans(zimbabwe_children_data[ , 4:13], na.rm=TRUE)


## Part (e) Calculating Cronbach's Alpha of the Overall Index  

survey_response <- zimbabwe_children_data[,4:13]
cronbach_alpha <- cronbach.alpha(survey_response, CI = T, na.rm = T)

# Creating a table to report Cronbach's Alpha 

ca_table <- data.frame(Value = c(cronbach_alpha[["alpha"]], 10, nrow(zimbabwe_children_data)))
rownames(ca_table) <- c("Cronbach's Alpha", "Number of Items in Index", "Number of Observations in Sample")

stargazer(ca_table, 
          summary = FALSE,
          type = "html",
          title = "Table 2: Reporting Reliability Coefficient of the Index", 
          digits = 2, 
          out = "table2.html")


## Part (f)  Plotting the conditional mean of the created index by the child's age in months

# Creating a variable for child's age in months 
zimbabwe_children_data$'child_age_months'<- interval(zimbabwe_children_data$child_birthday, zimbabwe_children_data$interview_date) %/% months(1)

# Relocating the new variable in the data frame
zimbabwe_children_data <- zimbabwe_children_data %>% 
  relocate(child_age_months, .after = child_age_years)

# Calculating conditional mean of overall index by child's age in months
conditional_mean <- zimbabwe_children_data %>%
  group_by(child_age_months) %>%
  summarise(mean_index = mean(Overall, na.rm = TRUE))

# Plotting conditional mean by child's age in months
plot_overall_index <- ggplot(data = conditional_mean, aes(x = child_age_months, y = mean_index)) +
  geom_line() + geom_point() +         
  labs(title = "Conditional Mean of Overall Index by Child's Age in Months",
       x = "Child's Age in Months",
       y = "Mean Overall Index") + theme_grey() + 
  theme(plot.title = element_text(size = 12))

# Saving the plot in HTML as an interactive plot
plot_overall_index <- ggplotly(plot_overall_index)
saveWidget(plot_overall_index, file = "plot_overall_index.html")


## Part (g)  Conditional mean plots for sub-indices 

## Calculating Sub-Indices 

# Literacy + Math Sub Index 
zimbabwe_children_data$'Literacy-Math' <- rowMeans(zimbabwe_children_data[ , c("EC6", "EC7", "EC8")], na.rm=TRUE)

# Physical Sub Index 
zimbabwe_children_data$'Physical' <- rowMeans(zimbabwe_children_data[ , c("EC9", "EC10")], na.rm=TRUE)

# Learning Sub Index 
zimbabwe_children_data$'Learning' <- rowMeans(zimbabwe_children_data[ , c("EC11", "EC12")], na.rm=TRUE)

# Socio-Emotional Sub Index 
zimbabwe_children_data$'Socio-Emotional' <- rowMeans(zimbabwe_children_data[ , c("EC13", "EC14", "EC15")], na.rm=TRUE)


# Defining a function to create conditional mean plots for all sub-indices
# Note: Run the following code from the function to the loop (line 146 to 179) together

conditional_mean_plot <- function(data, index, age, output_wd) {

# Calculating the conditional mean of the specified sub-index by child's age in months
conditional_mean <- data %>%
    group_by_at(age) %>%
    summarise(mean_subindex = mean(.data[[index]], na.rm = TRUE))
  
# Plotting conditional mean by child's age in months
plot <- ggplot(conditional_mean, aes_string(x = age, y = "mean_subindex")) +
    geom_line() + geom_point() +
    labs(title = paste("Conditional Mean of", index, " Sub-Index by Child's Age in Months"),
         x = "Child's Age in Months", y = paste("Mean", index, "Sub-Index")) +
    theme_grey() + theme(plot.title = element_text(size = 10))
  
# Saving the plot 
ggsave(filename = file.path(output_wd, paste0("plot_", index, "_subindex.jpeg")), plot = plot)

}

# List of variables to plot 
subindices <- c("Literacy-Math", "Physical", "Learning", "Socio-Emotional")

# Defining the output directory for the plots
output_wd <- "Sub-Indices Plots"

# Creating the directory if it doesn't exist
dir.create(output_wd, showWarnings = FALSE)

# Creating a loop to apply the function to each sub-index
for (index in subindices) {
  conditional_mean_plot(zimbabwe_children_data, index, "child_age_months", output_wd)
}


## Saving sub-indices plots in one html file

# Defining the plots path (based on where the sub-indices plots are stored)
setwd("/Users/anudhiisundaram/Desktop/World Bank/Empirical_Exercise_Output/Sub-Indices Plots")
plot_path <- c("plot_Learning_subindex.jpeg", "plot_Literacy-Math_subindex.jpeg", "plot_Physical_subindex.jpeg", 
                 "plot_Socio-Emotional_subindex.jpeg")

# Creating the HTML content
plot_html <- tags$html(
  tags$head(
    tags$title("Conditional Mean Plots")  
  ),
  tags$body(
    h1("Conditional Mean Plots for Sub-Indices by Child's Age in Months"), 
    lapply(plot_path, function(plot_path) {
      tags$div(
        tags$img(src = plot_path, style = "width:50%; height:auto;")  
      )
    })
  )
)

# Saving the plots in one HTML file
save_html(plot_html, file = "plots_subindices.html")



## Part (h) OLS Regression 

# Regressing overall index and sub-indices on child's age in months  
model_1 <- lm(Overall ~ child_age_months, data = zimbabwe_children_data)
model_2 <- lm(`Literacy-Math` ~ child_age_months, data = zimbabwe_children_data)
model_3 <- lm(Physical ~ child_age_months, data = zimbabwe_children_data)
model_4 <- lm(Learning ~ child_age_months, data = zimbabwe_children_data)
model_5 <- lm(`Socio-Emotional` ~ child_age_months, data = zimbabwe_children_data)

# Creating a regression table

lm_models <- list(model_1, model_2, model_3, model_4, model_5)

# Setting the working directory again to store the regression table in the main output folder
# Note: Change the working directory based on where the main output folder is
setwd("/Users/anudhiisundaram/Desktop/World Bank/Empirical_Exercise_Output")

stargazer(lm_models, 
          type = "html", 
          title = "Table 3: Regression Output", 
          column.labels = c("Overall", "Literacy+Math", "Physical", 
                            "Learning", "Socio-Emotional"),
          dep.var.caption = "Indices",
          dep.var.labels.include = FALSE,
          covariate.labels = "Child's Age in Months", 
          out = "table3.html")


###############################################################################################
########################## SECTION 2: CODE FOR QUESTION 3 #####################################

# Importing World Bank data for stunting in Zimbabwe, Lower Middle Income countries and Sub-Saharan Africa   
countries <- c("ZWE", "LMC", "SSF")
stunting_data <- wb_data("SH.STA.STNT.ME.ZS", country = countries, 
                         start_date = 2010, end_date = 2023)

# Plotting a line chart for comparison of prevalence of stunting among children under 5
plot_stunting <- ggplot(stunting_data, aes(x=date)) + geom_line(aes(y=SH.STA.STNT.ME.ZS, col=country)) + 
  geom_point(aes(y=SH.STA.STNT.ME.ZS, col=country), shape = 15) + 
  labs(title="Prevalence of Stunting in Zimbabwe, Lower Middle Income Countries and Sub-Saharan Africa", 
       x = "Year", y="% of children under 5 with stunting (height for age)", color=NULL, 
       subtitle = "Data from 2010 to 2022") + theme_grey() +
   theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 10)) 

# Saving the plot in HTML as an interactive plot
plot_stunting <- ggplotly(plot_stunting)
saveWidget(plot_stunting, file = "plot_stunting.html")

# Creating a table with prevalence of stunting in Zimbabwe, Lower Middle Income countries and Sub-Saharan Africa in 2022
stunting_data_recent <- stunting_data %>%
  filter(date==2022) %>%
  select(country, SH.STA.STNT.ME.ZS) %>% 
  rename('Prevalence of Stunting' = SH.STA.STNT.ME.ZS, 'Country/Region' = country)

stargazer(stunting_data_recent, 
          summary = FALSE, 
          type = "html", 
          title = "Table 4: Prevalence of Stunting (height for age) in 2022", 
          notes = "Note: Modeled estimates show the % of children under 5", 
          out = "table4.html")


###############################################################################################
####################### SECTION 3: ANSWER FOR QUESTION 4 ######################################

"
In 2022, while nearly 28% and 31% of children under 5 in Lower Middle Income countries and Sub Saharan 
Africa respectively were stunted, in Zimbabwe this figure was much lower at 21.6%. 
Comparing the prevalence of stunting over the last decade, in Zimbabwe it has consitently reduced 
from 33.5% in 2010 to 21.6% in 2022. In Lower Middle Income countries and Sub Saharan Africa as
well, the prevalence of stunting among children under 5 has reduced consistently between 2010 and 2022, 
however, in Zimbabwe, the reduction has been much more significant. In the last decade, 
the prevalance of stunting among children under 5 reduced by about 35% in Zimbabwe, while in 
Lower Middle Income countries and Sub Saharan Africa, it reduced by 26% and 16% respectively. 

We observe the positive impact of reduced stunting among children under 5 in Zimbabwe on the early 
childhood development indicators from the 2019 Zimbabwe MICS6 survey. 71% percent of children 
age 3-4 years were developmentally on track in at least three of the four domains (literacy-numeracy, 
physical, social-emotional, and learning domains). Besides, school attendance to an Early Childhood 
Education programme or primary school was 81%, with primary school net attendance being
very high (91%) as compared to lower secondary school net attendance (60%). Further, among
children age 2-4 years, only 2.5% reported to have difficulties controlling their
behaviours, compared with children of the same age.
"


############################### END OF SCRIPT ######################################################