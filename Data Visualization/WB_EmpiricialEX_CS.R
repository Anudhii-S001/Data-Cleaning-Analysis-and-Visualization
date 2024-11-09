## R Version: 4.3.2 

## Title: WB Empirical Exercise 

## Date Created: August 20, 2024
## Date Updated: August 25, 2024

## This R script was written as part of an empirical exercise by the World Bank. The script contains code on 
## data cleaning and visualization using World Bank Data and API. 

## Please note: This file is not reproducible because data cannot be made publicly available.  

### Clearing R environment 
rm(list=ls())

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

## Setting working directory
setwd("/Users/Desktop/Graphs and plots")

###### Part (a) Importing data for the Children under 5 interview 
zimbabwe_children_data <- as.data.frame(read_csv("~/Desktop/World Bank/EC2_empirical_exercise/01_rawdata/Zimbabwe_children_under5_interview.csv"))

###### Part (b) Recoding survey response for variables EC6 to EC15 such that Yes=1, No=0, and DK=0
zimbabwe_children_data <- zimbabwe_children_data %>%
  mutate(across(starts_with("EC"), ~ recode(.x, "1" = 1, "2" = 0, "8" = 0, "9" = 0)))

###### Part (c) Calculating the Overall Index using variables EC6 to EC15
zimbabwe_children_data$'Overall' <- rowMeans(zimbabwe_children_data[ , 4:13], na.rm=TRUE)

###### Part (d) Calculating Cronbach's Alpha of the Overall Index  
survey_response <- zimbabwe_children_data[,4:13]
cronbach_alpha <- cronbach.alpha(survey_response, CI = T, na.rm = T)

###### Part (e)  Plotting the conditional mean of the created index by the child's age in months

# Creating a variable for child's age in months 
zimbabwe_children_data$'child_age_months'<- interval(zimbabwe_children_data$child_birthday, zimbabwe_children_data$interview_date) %/% months(1)

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


###### Part (f)  Conditional mean plots for sub-indices 

### Calculating Sub-Indices 

# Literacy + Math Sub Index 
zimbabwe_children_data$'Literacy-Math' <- rowMeans(zimbabwe_children_data[ , c("EC6", "EC7", "EC8")], na.rm=TRUE)

# Physical Sub Index 
zimbabwe_children_data$'Physical' <- rowMeans(zimbabwe_children_data[ , c("EC9", "EC10")], na.rm=TRUE)

# Learning Sub Index 
zimbabwe_children_data$'Learning' <- rowMeans(zimbabwe_children_data[ , c("EC11", "EC12")], na.rm=TRUE)

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
subindices <- c("Literacy-Math", "Physical", "Learning")

# Defining the output directory for the plots
output_wd <- "Sub-Indices Plots"

# Creating the directory if it doesn't exist
dir.create(output_wd, showWarnings = FALSE)

# Creating a loop to apply the function to each sub-index
for (index in subindices) {
  conditional_mean_plot(zimbabwe_children_data, index, "child_age_months", output_wd)
}


### Saving sub-indices plots in one html file

# Defining the plots path (based on where the sub-indices plots are stored)
setwd("/Users/Desktop/Sub-Indices Plots")
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

###### Part (g) Using World Bank API to create comparison graph

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

############################### END OF SCRIPT ##################################################