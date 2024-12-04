# Analyzing Isotope Data
# Author: Poppy C. Northing
# Last edited: 03dec2024

# Load Libraries
library(tidyverse)
library(psych)
library(Hmisc)
library(PerformanceAnalytics)

# Load Data
isotope_data <- read.csv("Data/isotope/isotopes_with_metadata.csv", header = T)
str(isotope_data) # looks good

##############
### BASICS ###
##############

# generate summary statistics for each variable
isotope_summary <- describe(isotope_data)
write.csv(isotope_summary, "Data/isotope/isotope_stats_summary.csv", row.names = F)

# look at how data are distributed
isotope_data %>% ggplot() + geom_histogram(aes(x=WUE))
isotope_data %>% ggplot() + geom_histogram(aes(x=N_percent))
isotope_data %>% ggplot() + geom_histogram(aes(x=C.N))
isotope_data %>% ggplot() + geom_histogram(aes(x=mean_sla))

# are there significant differences in WUE or N content among sites?

isotope_data %>% ggplot() + aes(x = site.x, y = WUE, color = site.x) + geom_boxplot() + theme_classic() # quick visual
isotope_data %>% ggplot() + aes(x = site.x, y = N_percent, color = site.x) + geom_boxplot() + theme_classic() # quick visual
isotope_data %>% ggplot() + aes(x = site.x, y = C.N, color = site.x) + geom_boxplot() + theme_classic() # quick visual

m1 <- lm(WUE ~ site.x, data = isotope_data)
plot(m1)

sub <- subset(isotope_data, select = -c(precision, site.x, plant_ID))
c <- rcorr(as.matrix(sub[,2:8]))
chart.Correlation(sub[,2:8], histogram = TRUE, pch = 20)
