"
PROJECT NAME: INTERNATIONAL MIGRATION FROM BANGLADESH 
SUPERVISOR: DR. MAHREEN KHAN 

SUMMARY: SPATIAL MAPPING  

R Version: 4.3.2 

THIS R SCRIPT HAS THE FOLLOWING SECTIONS:
SECTION 1: DISTRICT WISE YEARLY MIGRATION (WITH SUBDISTRICT BOUNDARIES) 
SECTION 2: DIVISION WISE AND DISTRICT WISE MAPS OF BANGLADESH
SECTION 3: DISTRICT WISE YEARLY MIGRATION (WITH ONLY DISTRICT BOUNDARIES)
SECTION 4: DIVISION WISE YEARLY MIGRATION (WITH ONLY DIVISION BOUNDARIES)
SECTION 5: EXTRA SPATIAL MAPPING CODE FOR FUTURE REFERENCE 
"


################################################################################
#### SECTION 1. DISTRICT WISE YEARLY MIGRATION (WITH SUBDISTRICT BOUNDARIES) ###
################################################################################


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


### Importing data sets 
dist_mig_data <- read_dta("https://www.dropbox.com/scl/fi/ty557ulk6sp11qrk9sw1c/dist_instruments_202103.dta?rlkey=qhi09kcjefcfzbgmqdoloba0u&dl=1")
sf_data <- read_sf("bgd_admbnda_adm3_bbs_20180410.shp")

### DATA WRANGLING 

## 1. District migration data 

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

# Relocating variables
dist_mig_data <- dist_mig_data %>% 
  relocate(dist_ID, .before = dist_name)

## 2. Shape file data 

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
         
## Merging district migration and shape file data 

merged_data <- merge(sf_data, dist_mig_data, by = "dist_name")
  
# Relocating variables 
merged_data <- merged_data %>%
  relocate(dist_ID, .before = dist_name)

merged_data <- merged_data %>%
  relocate(subdist_name:ln_migs, .after = dist_name)

merged_data <- merged_data %>%
  relocate(dist_pcode, dist_smartyear, .before = subdist_name)  

### PLOTTING MAPS 

### Setting up working directory to store output files
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")


## 1. District wise yearly migration in Bangladesh in 2012 

jpeg("dist_mig_2012_subbound.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot1 <- merged_data %>% 
  filter(dist_smartyear == 2012) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
  theme_bw() + theme(legend.position="right", legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2012")
plot1
dev.off() 


## 2. District wise yearly migration in Bangladesh in 2015 

jpeg("dist_mig_2015_subbound.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot2 <- merged_data %>% 
  filter(dist_smartyear == 2015) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000)) + 
  theme_bw() + theme(legend.position="right", legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2015")
plot2
dev.off() 

# 3. Plotting both the maps side-by-side

plot1 <- merged_data %>% 
  filter(dist_smartyear == 2012) %>% # filter for year
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
  theme_bw() + theme(legend.position="right", legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=7), legend.text = element_text(size = 6),
  plot.title = element_text(size=10)) +  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill = "Yearly migration count") + ggtitle("Migration in 2012")

plot2 <- merged_data %>% 
  filter(dist_smartyear == 2015) %>% # filter for year
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000)) + 
  theme_bw() + theme(legend.position="right", legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=7), legend.text = element_text(size = 6),
  plot.title = element_text(size=10)) +  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Yearly migration count") + ggtitle("Migration in 2015")


jpeg("dist_mig_2012&2015_subbound.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot3 <- plot1 + plot2 + plot_annotation(title = "District wise yearly migration in Bangladesh", 
          theme = theme(plot.title = element_text(hjust = 0.45)))
plot3
dev.off()
################################################################################



################################################################################
########### SECTION 2. DIVISION WISE AND DISTRICT WISE MAPS OF BANGLADESH ######
################################################################################

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

# Loading shape file
sf_data <- st_read("bgd_admbnda_adm3_bbs_20180410.shp")

# Aggregating geometries by division
div_data <- sf_data %>%
  group_by(ADM1_EN) %>%
  summarize(geometry = st_union(geometry))

# Saving the result as a new shape file
st_write(div_data, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/division_bd_geometry.shp")

# Aggregating geometries by district
dist_data <- sf_data %>%
  group_by(ADM2_EN) %>%
  summarize(geometry = st_union(geometry))

# Saving the result as a new shape file
st_write(dist_data, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/district_bd_geometry.shp")

#### Plotting maps 

## Setting up working directory to store output files
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")

# Plotting division wise map of Bangladesh 
jpeg("division_map_bd.jpeg", pointsize = 2, width=1800, height=1200, res=200)
div_data %>%
  ggplot() + geom_sf(aes(fill = ADM1_EN)) + geom_sf_text(aes(label = ADM1_EN), size = 3) +
  theme(legend.key.size = unit(0.5, 'cm')) + labs(fill = "Division Name") + 
  ggtitle("Division wise map of Bangladesh")
dev.off()

# Plotting district wise map of Bangladesh 
jpeg("district_map_bd.jpeg", pointsize = 2, width=1800, height=1200, res=200)
dist_data %>%
  ggplot() + geom_sf(aes(fill = ADM2_EN)) + geom_sf_text(aes(label = ADM2_EN), size = 2) +
  theme(legend.key.size = unit(0.5, 'cm')) + labs(fill = "District Name") +
  ggtitle("District wise map of Bangladesh")
dev.off()

####################################################################################




################################################################################
### SECTION 3. DISTRICT WISE YEARLY MIGRATION (WITH ONLY DISTRICT BOUNDARIES) ##
################################################################################

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

# Importing district wise migration data set
dist_mig_data <- read_dta("https://www.dropbox.com/scl/fi/ty557ulk6sp11qrk9sw1c/dist_instruments_202103.dta?rlkey=qhi09kcjefcfzbgmqdoloba0u&dl=1")
# Loading shape file for district boundaries 
dist_data <- st_read("district_bd_geometry.shp")

### DATA WRANGLING 

## 1. District migration data 

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

# Relocating variables
dist_mig_data <- dist_mig_data %>% 
  relocate(dist_ID, .before = dist_name)

## 2. Shape file data 

# Renaming variable for ease of merge
dist_data <- dist_data %>%
  rename(dist_name = ADM2_EN)

## Merging district migration and shape file data 
merged_data <- full_join(dist_data, dist_mig_data, by = "dist_name")

# Relocating variables 
merged_data <- merged_data %>%
  relocate(dist_ID, .before = dist_name)

### PLOTTING MAPS 

### Setting up working directory to store output files
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")


## 1. District wise yearly migration in Bangladesh in 2012 

jpeg("dist_mig_2012.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot1 <- merged_data %>% 
  filter(dist_smartyear == 2012) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  legend.position="right", legend.key.size = unit(1.5, 'cm'),
  legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2012")
plot1
dev.off() 

"
scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
scale_fill_gradientn(colours = terrain.colors(7), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
scale_fill_viridis_c(option = plasma, direction = -1, breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) + 
scale_fill_distiller(palette = RdPu, breaks = c(10000, 20000, 30000, 40000, 50000, 60000))
"

## 2. District wise yearly migration in Bangladesh in 2015 

jpeg("dist_mig_2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot2 <- merged_data %>% 
  filter(dist_smartyear == 2015) %>% 
  ggplot() + geom_sf(aes(fill = dist_yrly_mig), color = NA) + 
  geom_sf_text(aes(label = dist_name), size = 1.5) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(10000, 20000, 30000, 40000, 50000, 60000)) +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
  legend.position="right", legend.key.size = unit(1.5, 'cm'),
  legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("District wise yearly migration in Bangladesh in 2015")
plot2
dev.off() 


# 3. Plotting both the maps side-by-side

plot1 <- merged_data %>% 
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

plot2 <- merged_data %>% 
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


jpeg("dist_mig_2012&2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot3 <- plot1 + plot2 + plot_annotation(title = "District wise yearly migration in Bangladesh", 
              theme = theme(plot.title = element_text(hjust = 0.45)))
plot3
dev.off()
#################################################################################





#################################################################################
### SECTION 4. DIVISION WISE YEARLY MIGRATION (WITH ONLY DIVISION BOUNDARIES) ##
################################################################################

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

# Importing district wise migration data set
dist_mig_data <- read_dta("https://www.dropbox.com/scl/fi/ty557ulk6sp11qrk9sw1c/dist_instruments_202103.dta?rlkey=qhi09kcjefcfzbgmqdoloba0u&dl=1")
# Loading shape file for district boundaries 
div_data <- st_read("division_bd_geometry.shp")
sf_data <- st_read("bgd_admbnda_adm3_bbs_20180410.shp")

### DATA WRANGLING 

## 1. District migration data 

# Dropping variables 
dist_mig_data <- dist_mig_data %>% 
  select(-`_est_m1`, -`_est_m2`, -area_km2, -pop_cen_11, -mean_occ, -mean_male, 
         -yrly_migs_bd, -ss, -ln_migs)

# Adding a prefix to all variables 
dist_mig_data <- dist_mig_data %>% 
  rename_with(.fn = ~ paste0("dist_", .x))

# Renaming variables 
dist_mig_data <- dist_mig_data %>%
  rename(dist_name = dist_giszila, dist_ID = dist_dist, 
        dist_yrly_mig = dist_yrly_migs_dist) 

# Relocating variables
dist_mig_data <- dist_mig_data %>% 
  relocate(dist_ID, .before = dist_name)

## 2. Sub district shape file data 

# Selecting relevant variables
sf_data <- sf_data %>% 
  select(ADM2_EN, ADM1_EN, geometry)

# Converting all variable names to lowercase 
sf_data <- rename_with(sf_data, tolower)

# Renaming variables 
sf_data <- sf_data %>% 
  rename(dist_name = adm2_en, div_name = adm1_en) 

## MERGING DATA SETS
merged_data <- full_join(dist_mig_data, sf_data, by = "dist_name", relationship = "many-to-many")

## 3. Division shape file data 

# Renaming variables 
div_data <- div_data %>%
  rename(div_name = ADM1_EN)

## MERGING DATA SETS
merged_data <- full_join(merged_data, div_data, by = "div_name")

# Dropping and renaming variables 
merged_data <- merged_data %>%
  select(-geometry.x) %>%
  rename(geometry = geometry.y) 

# Removing duplicate rows 
merged_data <- distinct(merged_data)

# Creating a data set for division wise migration in 2012
div_mig_data_2012 <- merged_data %>%
  filter(dist_smartyear == 2012) %>%
  group_by(div_name) %>%
  summarise(div_yrly_mig = sum(dist_yrly_mig))

div_mig_sf_2012 <- merge(div_mig_data_2012, div_data, by = "div_name")

# Saving the data set as a shape file 
st_write(div_mig_sf_2012, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/div_mig_sf_2012.shp")

# Creating a data set for division wise migration in 2015
div_mig_data_2015 <- merged_data %>%
  filter(dist_smartyear == 2015) %>%
  group_by(div_name) %>%
  summarise(div_yrly_mig = sum(dist_yrly_mig))

div_mig_sf_2015 <- merge(div_mig_data_2015, div_data, by = "div_name")

# Saving the data set as a shape file 
st_write(div_mig_sf_2015, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/div_mig_sf_2015.shp")

## Loading both shape files 

div_mig_sf_2012 <- st_read("div_mig_sf_2012.shp")
div_mig_sf_2015 <- st_read("div_mig_sf_2015.shp")

### Setting up working directory to store output files
# Change the working directory path based on where the output files should be stored. Add double inverted commas before and after the file path to run the command.
setwd("/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Output")


## 1. Division wise yearly migration in Bangladesh in 2012 

jpeg("div_mig_2012.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot1 <-  div_mig_sf_2012 %>%
  ggplot() + geom_sf(aes(fill = dv_yrl_), color = NA) + 
  geom_sf_text(aes(label = div_nam), size = 2) +
  scale_fill_gradientn(colours = rainbow(5)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
  legend.position="right", legend.key.size = unit(1.5, 'cm'),
  legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("Division wise yearly migration in Bangladesh in 2012")
plot1
dev.off() 

## 2. Division wise yearly migration in Bangladesh in 2015 

jpeg("div_mig_2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot2 <-  div_mig_sf_2015 %>%
  ggplot() + geom_sf(aes(fill = dv_yrl_), color = NA) + 
  geom_sf_text(aes(label = div_nam), size = 2) +
  scale_fill_gradientn(colours = rainbow(5)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
  legend.position="right", legend.key.size = unit(1.5, 'cm'),
  legend.title = element_text(size=10), plot.title = element_text(size=10)) +
  labs(fill = "Yearly migration count") + 
  ggtitle("Division wise yearly migration in Bangladesh in 2015")
plot2
dev.off() 


# 3. Plotting both the maps side-by-side

plot1 <- div_mig_sf_2012 %>%
  ggplot() + geom_sf(aes(fill = dv_yrl_), color = NA) + 
  geom_sf_text(aes(label = div_nam), size = 2) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(50000, 100000, 150000, 200000, 
  250000), limits = c(0, 250000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
  legend.position="right", legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 5),
  legend.title = element_text(size=7), plot.title = element_text(size=10)) +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Yearly migration count") + 
  ggtitle("Migration in 2012")

plot2 <- div_mig_sf_2015 %>%
  ggplot() + geom_sf(aes(fill = dv_yrl_), color = NA) + 
  geom_sf_text(aes(label = div_nam), size = 2) +
  scale_fill_gradientn(colours = rainbow(5), breaks = c(50000, 100000, 150000, 200000, 
  250000), limits = c(0, 250000)) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
  legend.position="right", legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 5),
  legend.title = element_text(size=7), plot.title = element_text(size=10)) +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Yearly migration count") + 
  ggtitle("Migration in 2015")

jpeg("div_mig_2012&2015.jpeg", pointsize = 2, width=1800, height=1200, res=200)
plot3 <- plot1 + plot2 + plot_annotation(title = "Division wise yearly migration in Bangladesh", 
        theme = theme(plot.title = element_text(hjust = 0.45)))
plot3
dev.off()
################################################################################


setwd("/Users/anudhiisundaram/Downloads/whosonfirst-data-admin-bd-latest")

# Division data - latitude and longitude 
data_division <- read_sf("whosonfirst-data-admin-bd-region-polygon.shp")

division_map <- plot(st_geometry(data_division), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 1)
division_map

# District data - latitude and longitude 
data_district <- read_sf("whosonfirst-data-admin-bd-county-polygon.shp")

district_map <- plot(st_geometry(data_district), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 1)
district_map

# Wards data? - latitude and longitude 
data_locality_point <- read_sf("whosonfirst-data-admin-bd-locality-point.shp")

locality_map <- plot(st_geometry(data_locality_point))
locality_map

data_neighbourhood <- read_sf("whosonfirst-data-admin-bd-neighbourhood-point.shp")


################################################################################
################ SECTION 5: EXTRA CODE FOR FUTURE REFERENCE ####################
################################################################################

########## Collapsing subdistrict coordinates to district coordinates 

subdistricts <- st_read("bgd_admbnda_adm3_bbs_20180410.shp")

# Check the structure and attributes of the subdistricts data
print(subdistricts)
# Make sure the geometry column contains polygons

# Define the district and year you want to combine
selected_district <- "Bagerhat"

# Filter the data based on district and year
selected_data <- subdistricts %>%
  filter(ADM2_EN == selected_district)

# Union or merge the selected data
larger_geometry <- st_union(selected_data$geometry)

# Check the result
print(larger_geometry)

# Save the result as a new shapefile
st_write(larger_geometry, "/Users/anudhiisundaram/Future of Development Dropbox/Anudhii Sundaram/Migration/Data/gps/BGD_UPAZILA_SHAPE FILES/larger_geometry.shp")


#################################################################################
########### CODE FOR CREATING SEPARATE SHAPEFILE FOR EACH DISTRICT IN BANGLADESH

# Load the shapefile containing subdistrict geometries
subdistricts <- st_read("bgd_admbnda_adm3_bbs_20180410.shp")

# Check the structure and attributes of the subdistricts data
print(subdistricts)

# Make sure the geometry column contains polygons

# Get unique district names
unique_districts <- unique(subdistricts$ADM2_EN)

# Create an empty list to store the combined geometries
combined_geometries <- list()

# Loop through each unique district
for (district in unique_districts) {
  # Filter the data for the current district
  district_data <- subdistricts %>%
    filter(ADM2_EN == district)
  
  # Union or merge the geometries for the current district
  combined_geometry <- st_union(district_data$geometry)
  
  # Add the combined geometry to the list
  combined_geometries[[district]] <- combined_geometry
}

# Save the combined geometries as separate shapefiles
for (i in 1:length(unique_districts)) {
  district <- unique_districts[i]
  st_write(combined_geometries[[i]], paste0("path/to/", district, "_combined.shp"))
}

"
a = st_sf(a = 1:3,
          geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
b = st_sf(a = 11:14,
          geom = st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3))))
st_join(a, b)
st_join(a, b, left = FALSE)
# two ways to aggregate y's attribute values outcome over x's geometries:
st_join(a, b) %>% aggregate(list(.$a.x), mean)
if (require(dplyr, quietly = TRUE)) {
  st_join(a, b) %>% group_by(a.x) %>% summarise(mean(a.y))
}
"
###################################################################################








