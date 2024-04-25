#Author: Poppy Northing
#Title: Analysis Script for PERE traits across Sites
#Last edited: 22FEB2024

#test change

#Load libraries
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
library(RColorBrewer)
library(gginnards)
library(ggthemr)
library(Hmisc)
library(PerformanceAnalytics)
library(broom)

#Load data
pere_data_mean <- read.csv("pere_data_mean.csv", header = TRUE)

# Analysis ----------------------------------------------------------------

  # Assess correlation between independent variables (elevation, VPD, temp, precip, SLA)
  c <- rcorr(as.matrix(pere_data_mean[,5:10])) # compute correlations and p-values

  chart.Correlation(pere_data_mean[,5:10], histogram = TRUE, pch = 20) # visualize

  #Check linear model assumptions
  model <- lm(mean_SLA ~ mean_temp + mean_precip + mean_VPD + elevation, data = pere_data_mean)
  model

  par(mfrow = c(2, 2)) #show all four plots to follow at once in a 2x2 grid
  plot(model) #gives all four diagnostic plots
  qqnorm(pere_data_mean$mean_SLA) #gives just qq plot

  #look at residuals visually
  model.diag.metrics <- augment(model)
  ggplot(model.diag.metrics, aes(mean_temp, mean_SLA)) +
    geom_point() +
    stat_smooth(method = lm, se = FALSE) +
    geom_segment(aes(xend = mean_temp, yend = .fitted), color = "red", size = 0.3)


mod1 <- lm(mean_SLA ~ site, data = pere_data_mean)
par(mfrow = c(2,2))
plot(mod1) #looks good

#Summary stats w/ means + std dev
aggregate(mean_SLA ~ site, data = pere_data_mean,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2))

#ANOVA
a1 <- aov(mean_SLA ~ site, data = pere_data)
summary(a1)
shapiro.test(a1$residuals)
hist(a1$residuals)

#POST-HOC tests
tukey1 <- TukeyHSD(a1, conf.level = 0.95)
plot(tukey1, las = 2)

letters <- multcompLetters4(a1, tukey1)
letters <- as.data.frame.list(letters$site)

emmeans(a1, specs = "site") %>% pairs()

#Linear modelling of individual environmental factors w/ mean and median SLA

#SLA ~ temp
lm_temp <- lm(mean_SLA ~ mean_temp, data = pere_data_mean)
summary(lm_temp)

lm_temp2 <- lm(median_SLA ~ mean_temp, data = pere_data_mean)
summary(lm_temp2)

#SLA ~ precipitation
lm_precip <- lm(mean_SLA ~ mean_precip, data = pere_data_mean)
summary(lm_precip)

lm_precip2 <- lm(median_SLA ~ mean_precip, data = pere_data_mean)
summary(lm_precip2)

#SLA ~ VPD
lm_VPD <- lm(mean_SLA ~ mean_VPD, data = pere_data_mean)
summary(lm_VPD)

lm_VPD2 <- lm(median_SLA ~ mean_VPD, data = pere_data_mean)
summary(lm_VPD2)

#SLA ~ elevation
lm_elevation <- lm(mean_SLA ~ elevation, data = pere_data_mean)
summary(lm_elevation)

lm_elevation2 <- lm(median_SLA ~ elevation, data = pere_data_mean)
summary(lm_elevation2)

#Hierarchical model comparison
m1 <- lm(mean_SLA ~ mean_precip, data = pere_data_mean)
summary(m1)


#Mixed effects modelling
#precipitation
me1a <- lmer(mean_SLA ~ mean_precip + (1|trip), data = pere_data_mean)
summary(me1a) #get singularity, which I think means that the random effect variance of "trip" is very close to zero and the data arne't "informative enough" to move it from 0.
anova(me1a, lm_precip) #compare AIC

#temperature
me1b <- lmer(mean_SLA ~ mean_temp + (1|trip), data = pere_data_mean)
summary(me1b)
anova(me1b, lm_temp) #compare AIC

#VPD
me1c <- lmer(mean_SLA ~ mean_VPD + (1|trip), data = pere_data_mean)
summary(me1c)

anova(me1c, lm_VPD)
#elevation
me1d <- lmer(mean_SLA ~ elevation + (1|trip), data = pere_data_mean)
summary(me1d)

anova(me1d, lm_elevation)

#Model comparison
lm1 <- lm(mean_SLA ~ 1, data = pere_data_mean)
lm2 <- lm(mean_SLA ~ mean_precip, data = pere_data_mean)
lm3 <- lm(mean_SLA ~ mean_precip + mean_temp, data = pere_data_mean)
lm4 <- lm(mean_SLA ~ mean_precip + mean_temp + elevation, data = pere_data_mean)
lm5 <- lm(mean_SLA ~ mean_precip*mean_temp*elevation, data = pere_data_mean)

anova(lm1, lm2, lm3, lm4, lm5)

# Visualization -----------------------------------------------------------

#For presentation on 19mar2024

    #SLA by site
    ggplot(pere_data) +
      aes(x = site, y = mean_SLA, color = site) +
      geom_jitter() +
      geom_boxplot(alpha = 0.5) +
      xlab("") + ylab("") +
      theme(axis.text = element_text(size=16)) +
      scale_color_brewer(palette ="Paired")

    ggplot(pere_data) +
      aes(x = site, y = mean_SLA, color = site) +
      geom_jitter() +
      scale_color_brewer(palette ="Paired")

    #SLA by elevation
    ep <- ggplot(pere_data_mean) +
      aes(x = elevation, y = mean_SLA) +
      geom_jitter(aes(color = site), size = 3) +
      xlab("") + ylab("") + ylim(12,22) +
      theme(legend.position="right") +
      theme(axis.text = element_text(size=20), plot.margin=unit(c(0.25, 0.25, 0.25, 0.25), "inches")) +
      scale_color_brewer(palette ="Paired"); ep

    #SLA by precipitation
    pp <- ggplot(pere_data_mean, aes(x= mean_precip, y = mean_SLA)) +
      geom_jitter(aes(fill = site), size = 3, shape = 21, color = "black") +
      geom_smooth(method = lm) +
      xlab("") + ylab("") + ylim(8,27) +
      theme(legend.position="left") +
      theme(axis.text = element_text(size=20), plot.margin=unit(c(0.25, 0.25, 0.25, 0.25), "inches")) +
      scale_color_brewer(palette ="Set3"); pp

    pp <- pp + geom_jitter(data = pere_data, aes(color = site), alpha = 0.3) + scale_color_brewer(palette ="Paired")

    move_layers(pp, "GeomPoint", position = "bottom")



    ppp <- ggplot(pere_data, aes(x = mean_precip, y = mean_SLA)) +
      geom_jitter(aes(color = site), width = 2.5) +
      stat_summary(fun.data = mean_se, color = "black", shape = 21, size = 1) +
      xlab("") + ylab("") +
      theme(axis.text = element_text(size=16)) +
      scale_color_brewer(palette ="Set3"); ppp

    ppp + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", color = "black")

    #SLA by VPD
    vp <- ggplot(pere_data_mean, aes(x= mean_VPD, y = mean_SLA)) +
      geom_jitter(aes(color = site), size = 3) +
      xlab("") + ylab("") + ylim(12,22) +
      theme(legend.position="none") +
      theme(axis.text = element_text(size=20), plot.margin=unit(c(0.25, 0.25, 0.25, 0.25), "inches")) +
      scale_color_brewer(palette ="Paired"); vp

    #SLA by winter temperature
    tp <- ggplot(pere_data_mean, aes(x= mean_temp, y = mean_SLA)) +
      geom_jitter(aes(color = site), size = 3) +
      xlab("") + ylab("") + ylim(12,22) + xlim(10,18) +
      theme(legend.position="none") +
      theme(axis.text = element_text(size=20), plot.margin=unit(c(0.25, 0.25, 0.25, 0.25), "inches")) +
      scale_color_brewer(palette ="Paired"); tp

    #putting plots together
    legend <- get_legend(ep)
    ep <- ep + theme(legend.position="none")
    plot_grid(ep, pp, vp, tp, nrow = 2, ncol = 2)
    grid.arrange(ep, pp, vp, tp, nrow = 2, ncol = 2,
                 widths = c(2.5, 2.5))

#For GPSC proposal
ggplot(data = pere_data_mean, aes(x = mean_precip, y = mean_SLA)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Mean Total Winter Precipitation (mm)") + ylab("Mean Specific Leaf Area (mm2/mg)") +
  theme_classic()

?plot

plot(x = pere_data_mean$mean_precip, y = pere_data_mean$mean_SLA,
     xlab = "Mean Winter Precipitation (mm)",
     ylab = "Mean Specific Leaf Area (mm2/2mg)")

abline(lm(pere_data_mean$mean_SLA ~ pere_data_mean$mean_precip))
