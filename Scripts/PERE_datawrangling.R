# Preparing raw data for analysis
# Poppy Northing
# pcnorthing@arizona.edu
# 09apr2024

# Load Libraries
library(dplyr)

# Load data
pere_data <- read.csv("final_SLA_data_12mar2024.csv", header = TRUE)
str(pere_data)

#remove timestamp column
pere_data <- subset(pere_data, select = -c(Timestamp))

#remove all rows with an NA in at least one column
pere_data <- pere_data[complete.cases(pere_data),]

#make a new column with just the site acronym
pere_data <- pere_data %>% mutate(site = substr(ID, start = 1, stop = 2))

#make a new column with the mean SLA for each leaf
pere_data <- pere_data %>% group_by(site, Plant) %>% mutate(mean_SLA = mean(SLA_.mm..mg.))

pere_data_mean <- pere_data %>% group_by(site, order, trip) %>% summarize(mean_SLA = mean(SLA_.mm..mg.),
                                                                          median_SLA = median(SLA_.mm..mg.),
                                                                          mean_temp = mean(mean_temp),
                                                                          mean_precip = mean(mean_precip),
                                                                          mean_VPD = mean(mean_VPD),
                                                                          elevation = mean(elevation))

# Write cleaned data to new csv
write.csv(pere_data_mean, file = "pere_data_mean.csv")



#get site climate data
pere_site_climate <- pere_data %>% group_by(site) %>% summarize(temp = mean(mean_temp),
                                                                precip = mean(mean_precip),
                                                                VPD = mean(mean_VPD),
                                                                elevation = mean(elevation))