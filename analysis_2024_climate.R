#Analysis test w/ 2024 climate data
#Author: Poppy Northing
#Created: 08mar2024

library(tidyverse)
library(multcomp)
library(multcompView)
library(ggcorrplot)
library(FactoMineR)
library(devtools)
library(factoextra)
library(ggfortify)
library(AICcmodavg)
library(lme4)
library(lmerTest)
library(emmeans)
library(glmm)
library(cowplot)

pere_data <- read.csv("prelim_sla_data_2024_climate.csv", header = T)

summary(pere_data)

#linear modelling

#SLA x mean temp

lm1 <- lm(mean_SLA ~ X2024_mean_temp, data = pere_data)
summary(lm1)

#SLA x min temp

lm2 <- lm(mean_SLA ~ X2024_mean_min_temp, data = pere_data)
summary(lm2)

#SLA x max temp

lm3 <- lm(mean_SLA ~ X2024_mean_max_temp, data = pere_data)
summary(lm3)

#SLA x total precip

lm4 <- lm(mean_SLA ~ X2024_mean_total_precip, data = pere_data)
summary(lm4)

#SLA x maxVPD

lm5 <- lm(mean_SLA ~ X2024_mean_maxvpd, data = pere_data)
summary(lm5)

#SLA x dewpoint

lm6 <- lm(mean_SLA ~ X2024_mean_dewpoint, data = pere_data)
summary(lm6)