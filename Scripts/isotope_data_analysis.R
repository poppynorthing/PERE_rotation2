# Analyzing Isotope Data
# Author: Poppy C. Northing
# Last edited: 03dec2024

# Load Libraries
library(tidyverse)
library(psych)

# Load Data
isotope_data <- read.csv("Data/isotope/isotopes_with_metadata.csv", header = T)
str(isotope_data) # looks good

##############
### BASICS ###
##############

isotope_summary <- describe(isotope_data)
write.csv(isotope_summary, "Data/isotope/isotope_stats_summary.csv", row.names = F)
