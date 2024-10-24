"
PROJECT NAME: INTERNATIONAL MIGRATION FROM BANGLADESH 
SUPERVISOR: DR. MAHREEN KHAN 

SUMMARY: DATA VISUALIZATION AND HEATMAPS 

R Version: 4.3.2 

THIS R SCRIPT HAS THE FOLLOWING SECTIONS: 

SECTION 1.1 AGE-SEX PYRAMID 
SECTION 1.2 HEATMAPS
SECTION 1.3 DIFFERENCE BETWEEN REGISTRATION AND ISSUANCE DATA OVER TIME
"

### Clearing the environment 
rm(list=ls())


### Setting up working directory to store the output files 
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")


### Installing libraries 
# If a package is not already installed then use install.packages("") to install the library first
library(ggplot2)
library(haven)
library(tidyverse)
library(repmis)
library(apyramid)
library(RColorBrewer)

### Importing data set 
raw_data <- read_dta("https://www.dropbox.com/scl/fi/c0cvglx6axncnfcdaig77/bmet_2009_2019_100k_20240308.dta?rlkey=7nh51ghwfh9fdy1rvh1a8g1ba&dl=1")



################################# 1.1 AGE-SEX PYRAMID OF THE SAMPLE

# Creating a new age variable to set observation values greater than 100 'NA'
raw_data$'AGE_NEW'<- ifelse(raw_data$AGE1 > 100, NA, raw_data$AGE1)

# Relocating the new variable in the data frame 
raw_data <- raw_data %>%
  relocate(AGE_NEW, .after = AGE1)

# Creating a new age group variable 
raw_data <- raw_data %>% 
  mutate(AGE_GROUP = case_when(AGE_NEW < 10 ~ "<10",
                               AGE_NEW >= 10 & AGE_NEW < 20 ~ "10-19",
                               AGE_NEW >= 20 & AGE_NEW < 30 ~ "20-29",
                               AGE_NEW >= 30 & AGE_NEW < 40 ~ "30-39",
                               AGE_NEW >= 40 & AGE_NEW < 50 ~ "40-49",
                               AGE_NEW >= 50 & AGE_NEW < 60 ~ "50-59",
                               AGE_NEW >= 60 ~ ">=60"))

# Converting the new age group variable into a factor 
raw_data$AGE_GROUP <- factor(raw_data$AGE_GROUP, levels = c("<10", "10-19", "20-29",
                                            "30-39","40-49","50-59",">=60"))

# Relocating the new variable in the data frame 
raw_data <- raw_data %>%
  relocate(AGE_GROUP, .after = AGE_NEW)

### Creating an age-sex pyramid of the sample 
jpeg("age_pyramid_plot.jpeg", width=2500, height=1500, res=200)
age_pyramid_plot <- age_pyramid(raw_data, AGE_GROUP, split_by = SEX) + 
            theme_minimal() + labs( x = "Age group (years)", y = "Number of people",
           fill    = "SEX", title   = "Age-Sex Pyramid") + scale_fill_brewer(palette = "Blues") 
age_pyramid_plot
dev.off()



################################# 1.2 DIFFERENCE BETWEEN REGISTRATION AND ISSUANCE DATA OVER TIME

### Clearing the environment 
rm(list=ls())

### Setting up working directory to store the output files 
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")

### Installing libraries 
# If a package is not already installed then use install.packages("") to install the library first
library(ggplot2)
library(haven)
library(tidyverse)
library(repmis)
library(viridis)

### Importing data set 
raw_data <- read_dta("https://www.dropbox.com/scl/fi/f92qf3f1fasxo98nwv2s6/raw_data_revised.dta?rlkey=xolblh8y98nw863o8oey39g2s&dl=1")

## Finding average delays between registration and issuance for every division annually from 2010 - 2019

# Creating a function to find average delays for every division 

mean_date_diff = function (x) {
  
  raw_data %>%
    filter(division == x) %>%
    group_by(regyear) %>%
   summarise_at(vars(date_diff), list(mean_date_diff = mean))
  
}

# Applying the function to each division 

# 1. Khulna
khulna_data <- mean_date_diff("Khulna")
# Creating a separate data set
khulna_data <- khulna_data %>%
  mutate(division = "Khulna", .before = regyear)

# 2. Sylhet
sylhet_data <- mean_date_diff("Sylhet")
# Creating a separate data set
sylhet_data <- sylhet_data %>%
  mutate(division = "Sylhet", .before = regyear)

# 3. Chittagong
chittagong_data <- mean_date_diff("Chittagong")
# Creating a separate data set
chittagong_data <- chittagong_data %>%
  mutate(division = "Chittagong", .before = regyear)

# 4. Dhaka
dhaka_data <- mean_date_diff("Dhaka")
# Creating a separate data set
dhaka_data <- dhaka_data %>%
  mutate(division = "Dhaka", .before = regyear)

# 5. Barishal
barishal_data <- mean_date_diff("Barishal")
# Creating a separate data set
barishal_data <- barishal_data %>%
  mutate(division = "Barishal", .before = regyear)

# 6. Rajshahi
rajshahi_data <- mean_date_diff("Rajshahi")
# Creating a separate data set
rajshahi_data <- rajshahi_data %>%
  mutate(division = "Rajshahi", .before = regyear)


## Appending all division average delay data sets 
mean_data_diff_data <- rbind(khulna_data, sylhet_data, chittagong_data, dhaka_data,
                             barishal_data, rajshahi_data)


### Creating a line chart for average delays in smart card issuance across divisions from 2010-19

jpeg("avg_delay_linechart.jpeg", width=2500, height=1500, res=200)
mean_data_diff_data %>%
  ggplot(aes(x= regyear, y=mean_date_diff, group = division, color = division)) +
  geom_line() + geom_point() +   theme_minimal() +
  labs(x = "Year of registration", y = "Average delay in smart card issuance (in days)", 
  title = "Comparison of average delays in smart card issuance across divisions from 2010-19")
dev.off()
