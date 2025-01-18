# Analyzing Isotope Data
# Author: Poppy C. Northing
# Last edited: 11dec2024

# Load Libraries
library(tidyverse)
library(psych)
library(Hmisc)
library(PerformanceAnalytics)
library(performance)
library(lme4)

# Load Data
isotope_data <- read.csv("Data/isotope/isotopes_with_metadata.csv", header = T)
isotope_data$site.x <- as.factor(isotope_data$site.x)
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

isotope_data %>% ggplot() + aes(x = site.x, y = WUE, color = site.x) + geom_boxplot() + theme_classic() # quick visual
isotope_data %>% ggplot() + aes(x = site.x, y = N_percent, color = site.x) + geom_boxplot() + theme_classic() # quick visual
isotope_data %>% ggplot() + aes(x = site.x, y = C.N, color = site.x) + geom_boxplot() + theme_classic() # quick visual

# look at correlations among dependent variables
sub <- subset(isotope_data, select = -c(precision, site.x, plant_ID))
c <- rcorr(as.matrix(sub[,2:8]))
chart.Correlation(sub[,2:8], histogram = TRUE, pch = 20)

# look at correlations among independent variables
sub2 <- subset(isotope_data, select = c(temp, precip, elev, vpd))
c <- rcorr(as.matrix(sub2))
chart.Correlation(sub2, histogram = TRUE, pch = 20)

################
### MODELING ###
################

# set up zero-sum contrast (ie, comparing each site to the overall mean)
# adopted from https://github.com/ejolly/R/blob/master/Guides/Contrasts_in_R.md
contrasts(isotope_data$site.x) <- contr.sum(11)
contrasts(isotope_data$site.x)

  # Inversion function for zero-sum contrast matrix
  contr.weights <- function(contr){
    contr <- cbind(intercept=1,contr)
    return(solve(contr))
  }

contr.weights(contrasts(isotope_data$site.x))

# basic linear models with site as a predictor for WUE & N content, respectively

m0_wue <- lm(WUE ~ site.x, data = isotope_data) # run model
check_model(m0_wue) # check model meets assumptions
summary(m0_wue) # see model output

m0_n <- lm(N_percent ~ site.x, data = isotope_data) # run model
check_model(m0_n) # check model meets assumptions
summary(m0_n) # see model output

# another way of getting to this information is with an anova / post-hoc tukey test
a1 <- aov(WUE ~ site.x, data = isotope_data)
summary(a1)

tukey1 <- TukeyHSD(a1, conf.level = 0.95)
plot(tukey1, las = 2)

letters <- multcompLetters4(a1, tukey1)
letters <- as.data.frame.list(letters$site)

emmeans(a1, specs = "site.x") %>% pairs()


# Now I want to add in environmental predictors to understand how climate drives trait differences

isotope_mod_data <- isotope_data %>% group_by(site.x) %>% summarise(
                                               mean_WUE = mean(WUE),
                                               mean_N_percent = mean(N_percent),
                                               mean_C.N = mean(C.N),
                                               mean_SLA = mean(mean_sla),
                                               temp = mean(temp),
                                               precip = mean(precip),
                                               elev = mean(elev),
                                               vpd = mean(vpd))


m1 <- lm(mean_WUE ~ precip, data = isotope_mod_data)
check_model(m1)
summary(m1)

m2 <- lm(mean_WUE ~ temp, data = isotope_mod_data)
check_model(m2)
summary(m2)

m3 <- lm(mean_WUE ~ vpd, data = isotope_mod_data)
check_model(m3)
summary(m3)

m4 <- lm(mean_WUE ~ elev, data = isotope_mod_data)
check_model(m4)
summary(m4)

m5 <- lm(mean_WUE ~ precip + temp + vpd, data = isotope_mod_data)
check_model(m5)
summary(m5)

mn1 <- lm(N_percent ~ precip, data = isotope_data)
check_model(mn1)
summary(mn1)

mn2 <- lm(N_percent ~ temp, data = isotope_data)
check_model(mn2)
summary(mn2)

mn3 <- lm(N_percent ~ vpd, data = isotope_data)
check_model(mn3)
summary(mn3)

mn4 <- lm(N_percent ~ elev, data = isotope_data)
check_model(mn4)
summary(mn4)

mcn5 <- lm(N_percent ~ precip + temp + elev, data = isotope_data)
check_model(mn5)
summary(mn5)

mcn1 <- lm(log(C.N) ~ precip, data = isotope_data)
check_model(mcn1)
summary(mcn1)

mcn2 <- lm(log(C.N) ~ temp, data = isotope_data)
check_model(mcn2)
summary(mcn2)

mcn3 <- lm(log(C.N) ~ vpd, data = isotope_data)
check_model(mcn3)
summary(mcn3)

mcn4 <- lm(log(C.N) ~ elev, data = isotope_data)
check_model(mcn4)
summary(mcn4)

mcn5 <- lm(C.N ~ precip + temp + elev, data = isotope_data)
check_model(mcn5)
summary(mcn5)
