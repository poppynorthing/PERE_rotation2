# Title: Preparing raw data for analysis
# Author: Poppy Northing; pcnorthing@arizona.edu
# Last edited: 03 DEC 2024

# Load Libraries
library(tidyverse)

# Load data
pere_sla_data <- read.csv("Data/final_SLA_data_12mar2024.csv", header = TRUE)
pere_isotope_data <- read.csv("Data/isotope/processed_isotope_data.csv", header = TRUE)
pere_metadata <- read.csv("Data/isotope/sample_metadata.csv", header = TRUE)
pere_metadata <- pere_metadata[,1:5] # remove extra weird columns to the right

################
### SLA DATA ###
################

# remove time stamp column
pere_sla_data <- subset(pere_sla_data, select = -c(Timestamp))

# remove all rows with an NA in at least one column
pere_sla_data <- pere_sla_data[complete.cases(pere_sla_data),]

# make a new column with just the site acronym
pere_sla_data <- pere_sla_data %>% mutate(site = substr(ID, start = 1, stop = 2))

# make a new column with the mean SLA for each leaf
pere_sla_data <- pere_sla_data %>% group_by(site, Plant) %>% mutate(mean_SLA = mean(SLA_.mm..mg.))

pere_sla_data_mean <- pere_sla_data %>% group_by(site, order, trip) %>% summarize(mean_SLA = mean(SLA_.mm..mg.),
                                                                          median_SLA = median(SLA_.mm..mg.),
                                                                          mean_temp = mean(mean_temp),
                                                                          mean_precip = mean(mean_precip),
                                                                          mean_VPD = mean(mean_VPD),
                                                                          elevation = mean(elevation))

# Write cleaned data to new csv
write.csv(pere_sla_data_mean, file = "Data/SLA/pere_sla_data_mean.csv", row.names=FALSE)


####################
### ISOTOPE DATA ###
####################

# First, I want to append population information, sample ID, sample weight, SLA, and climate data to my isotope data frame

# add metadata
pere_isotope_data_full <- pere_isotope_data %>% left_join(x = pere_isotope_data, y = pere_metadata, by = "sample")

# add SLA

plant_sla <- pere_sla_data %>% group_by(site, Plant) %>%
  summarise(mean_sla = mean(SLA_.mm..mg.), temp = mean(mean_temp), precip = mean(mean_precip), elev = mean(elevation), vpd = mean(mean_VPD)) %>% # get SLA summarized per plant
  mutate(plant_ID = paste(site, Plant, sep = "")) # add column with plant ID format like isotope metadata

pere_isotope_data_fuller <- pere_isotope_data_full %>% left_join(y = plant_sla, by = "plant_ID") # add SLA data

# add a column for N mass
pere_isotope_data_fuller <- pere_isotope_data_fuller %>% mutate(N_mass = N_percent * combined_weight / 100)

# now, remove columns that I don't need

final_isotope_data <- pere_isotope_data_fuller %>% subset(select = -c(site.y, leaves, Plant))

write.csv(final_isotope_data, file = "Data/isotope/isotopes_with_metadata.csv", row.names=FALSE) # make a file to save this data frame to

####################
### CLIMATE DATA ###
####################

# get site climate data
pere_site_climate <- pere_sla_data %>% group_by(site) %>% summarize(temp = mean(mean_temp),
                                                                precip = mean(mean_precip),
                                                                VPD = mean(mean_VPD),
                                                                elevation = mean(elevation))