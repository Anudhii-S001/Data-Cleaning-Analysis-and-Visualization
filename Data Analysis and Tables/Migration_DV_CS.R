## R Version: 4.3.2 

## Title: Data Visualization - Spatial Mapping 

## Date Created: May 07, 2024
## Date Updated: May 30, 2024

## This R script has the following sections: 
#    Section 1: Data Wrangling and Merge
#    Section 2: Creating shape files for district and division boundaries
#    Section 3: District wise annual migration 

## Brief Project Description: 
# The aim of the project is to examine the spillover effects of international labor migration on the labor and 
# socio-economic outcomes of non-migrant households in Bangladesh. 

## Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  


#######################################################################################################
############################### SECTION 1. DATA WRANGLING AND MERGE ###################################
#######################################################################################################

### Clearing R environment 
rm(list=ls())

### Setting up working directory to import shape file 
# Change the working directory path based on where the shape file is stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES")

### Loading libraries (if package not already installed then use install.packages() to install the library first)
library(haven)
library(repmis)
library(ggplot2)
library(sf)
library(tidyverse)
library(patchwork)

### Importing data sets 
dist_mig_data <- read_dta("https://www.dropbox.com/scl/fi/ty557ulk6sp11qrk9sw1c/dist_instruments_202103.dta?rlkey=qhi09kcjefcfzbgmqdoloba0u&dl=1")
sf_data <- read_sf("bgd_admbnda_adm3_bbs_20180410.shp")

################################## 1.1 District Migration Data 

# Dropping variables 
dist_mig_data <- dist_mig_data %>% 
  select(-`_est_m1`, -`_est_m2`)

# Adding a prefix to all variables 
dist_mig_data <- dist_mig_data %>% 
  rename_with(.fn = ~ paste0("dist_", .x))

# Renaming variables 
dist_mig_data <- dist_mig_data %>%
  rename(dist_name = dist_giszila, dist_ID = dist_dist, 
         bd_yrly_mig = dist_yrly_migs_bd, dist_yrly_mig = dist_yrly_migs_dist, 
         ln_migs = dist_ln_migs)

################################## 1.2 Shape File Data 

# Dropping variables 
sf_data <- sf_data %>% 
  select(-validOn, -validTo, -date, -ADM3_REF, -ADM3ALT1EN, -ADM3ALT2EN, 
         -ADM0_EN, -ADM0_PCODE)

# Converting all variable names to lowercase 
sf_data <- rename_with(sf_data, tolower)

# Renaming variables 
sf_data <- sf_data %>% 
  rename(subdist_name = adm3_en, subdist_pcode = adm3_pcode,
         dist_name = adm2_en, dist_pcode = adm2_pcode, 
         div_name = adm1_en, div_pcode = adm1_pcode) 

################################## 1.3 Merging district migration and shape file data 

merged_data <- merge(sf_data, dist_mig_data, by = "dist_name")


#######################################################################################################
########### SECTION 2. CREATING SHAPE FILES FOR DISTRICT AND DIVISION BOUNDARIES ######################
#######################################################################################################

################################## 2.1 Creating Shape Files 

## Loading shape file
sf_data <- st_read("bgd_admbnda_adm3_bbs_20180410.shp")

## Aggregating geometries by division to create a shape file with division coordinates
div_data <- sf_data %>%
  group_by(ADM1_EN) %>%
  summarize(geometry = st_union(geometry))

## Saving the data as a new shape file
st_write(div_data, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/division_bd_geometry.shp")

## Aggregating geometries by district to create a shape file with district coordinates
dist_data <- sf_data %>%
  group_by(ADM2_EN) %>%
  summarize(geometry = st_union(geometry))

# Saving the data as a new shape file
st_write(dist_data, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/district_bd_geometry.shp")

################################## 2.2 Plotting maps of Bangladesh using the new shape files 

# Plotting division wise map of Bangladesh and saving as a jpeg image
jpeg("division_map_bd.jpeg", pointsize = 2, width=1800, height=1200, res=200)
div_data %>%
  ggplot() + geom_sf(aes(fill = ADM1_EN)) + geom_sf_text(aes(label = ADM1_EN), size = 3) +
  theme(legend.key.size = unit(0.5, 'cm')) + labs(fill = "Division Name") + 
  ggtitle("Division wise map of Bangladesh")
dev.off()

# Plotting district wise map of Bangladesh and saving as a jpeg image
jpeg("district_map_bd.jpeg", pointsize = 2, width=1800, height=1200, res=200)
dist_data %>%
  ggplot() + geom_sf(aes(fill = ADM2_EN)) + geom_sf_text(aes(label = ADM2_EN), size = 2) +
  theme(legend.key.size = unit(0.5, 'cm')) + labs(fill = "District Name") +
  ggtitle("District wise map of Bangladesh")
dev.off()


#######################################################################################################
############################### SECTION 3. DISTRICT WISE ANNUAL MIGRATION #############################
#######################################################################################################

### Clearing the environment 
rm(list=ls())

### Setting up working directory to import shape file 
# Change the working directory path based on where the shape file is stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES")

### Load libraries 
library(haven)
library(repmis)
library(ggplot2)
library(sf)
library(tidyverse)
library(patchwork)

# Importing district wise migration data 
dist_mig_data <- read_dta("https://www.dropbox.com/scl/fi/ty557ulk6sp11qrk9sw1c/dist_instruments_202103.dta?rlkey=qhi09kcjefcfzbgmqdoloba0u&dl=1")
# Loading shape file for district boundaries 
dist_data <- st_read("district_bd_geometry.shp")

################################## 3.1 Data wrangling and merge  

# Dropping variables from district migration data
dist_mig_data <- dist_mig_data %>% 
  select(-`_est_m1`, -`_est_m2`)

# Adding a prefix to all variables in district migration data
dist_mig_data <- dist_mig_data %>% 
  rename_with(.fn = ~ paste0("dist_", .x))

# Renaming variables in district migration data
dist_mig_data <- dist_mig_data %>%
  rename(dist_name = dist_giszila, dist_ID = dist_dist, 
         bd_yrly_mig = dist_yrly_migs_bd, dist_yrly_mig = dist_yrly_migs_dist, 
         ln_migs = dist_ln_migs)

# Renaming variable in district shape file data 
dist_data <- dist_data %>%
  rename(dist_name = ADM2_EN)

## Merging district migration and shape file data 
merged_data <- full_join(dist_data, dist_mig_data, by = "dist_name")

################################## 3.2 Plotting maps for district wise annual migration in Bangladesh

####### 3.2.1 Plotting the map for district wise yearly migration in Bangladesh in 2012 and saving as a jpeg image
jpeg("dist_mig_2012.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot_1 <- merged_data %>% 
  filter(dist_smartyear == 2012) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                     legend.position="right", legend.key.size = unit(1.5, 'cm'),
                     legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2012")
plot_1
dev.off() 


####### 3.2.2 Plotting the map for district wise yearly migration in Bangladesh in 2015 and saving as a jpeg image
jpeg("dist_mig_2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot_2 <- merged_data %>% 
  filter(dist_smartyear == 2015) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                     legend.position="right", legend.key.size = unit(1.5, 'cm'),
                     legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2015")
plot_2
dev.off() 

####### 3.2.3 Plotting both the maps side-by-side

plot_1 <- merged_data %>% 
  filter(dist_smartyear == 2012) %>% # filter for year
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 
                                                        50000, 60000, 70000, 80000), limits = c(0, 80000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                     legend.position="right", legend.key.size = unit(0.75, 'cm'),
                     legend.title = element_text(size=6.5), legend.text = element_text(size = 5),
                     plot.title = element_text(size=10)) +  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill = "Yearly migration count") + ggtitle("Migration in 2012")

plot_2 <- merged_data %>% 
  filter(dist_smartyear == 2015) %>% # filter for year
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 
                                                        50000, 60000, 70000, 80000), limits = c(0, 80000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                     legend.position="right", legend.key.size = unit(0.75, 'cm'), 
                     legend.title = element_text(size=6.5), legend.text = element_text(size = 5),  
                     plot.title = element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Yearly migration count") + ggtitle("Migration in 2015")


# Saving the combined plot as a jpeg image
jpeg("distmig_map_2012&2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot_3 <- plot_1 + plot_2 + plot_annotation(title = "District wise yearly migration in Bangladesh", 
                                         theme = theme(plot.title = element_text(hjust = 0.45)))
plot_3
dev.off()

################################### END OF SCRIPT #################################################