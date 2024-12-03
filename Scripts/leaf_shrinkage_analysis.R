#Author: Poppy Northing
#PERE Leaf Shrinkage Experiment Analysis

shrinkage <- read.csv("compiled_leaf_shrinkage_data.csv")

library(lme4)
library(tidyverse)

shrinkage_model <- lm(shrinkage$X..shrinkage ~ shrinkage$Initial.Area..mm.., data = shrinkage)
summary(shrinkage_model)

#Looks like initial area significantly explains shrinkage %... alas. Effect size: -0.4236, p = 0.0414.