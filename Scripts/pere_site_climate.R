#Author: Poppy Northing
#Last edited: 24jan2024

library(tidyverse)
library(ggcorrplot)

#load data
sites <- read.csv("/Users/pnorthing/Documents/grad_school/Data/PRISM_2024winter_allsites_data.csv")
sites

#data cleanup

#get only months that I want (oct - mar) (when dealing with multiple seasons)
sites <- sites %>% filter(grepl('-10|-11|-12|-01|-02|-03', Date))

#add seasons (for dealing w/ multiple seasons)
sites <- sites %>% mutate(Season = case_when(
  grepl(x = Date, pattern = "13-1") ~ "13-14",
  grepl(x = Date, pattern = "14-0") ~ "13-14",
  grepl(x = Date, pattern = "14-1") ~ "14-15",
  grepl(x = Date, pattern = "15-0") ~ "14-15",
  grepl(x = Date, pattern = "15-1") ~ "15-16",
  grepl(x = Date, pattern = "16-0") ~ "15-16",
  grepl(x = Date, pattern = "16-1") ~ "16-17",
  grepl(x = Date, pattern = "17-0") ~ "16-17",
  grepl(x = Date, pattern = "17-1") ~ "17-18",
  grepl(x = Date, pattern = "18-0") ~ "17-18",
  grepl(x = Date, pattern = "18-1") ~ "18-19",
  grepl(x = Date, pattern = "19-0") ~ "18-19",
  grepl(x = Date, pattern = "19-1") ~ "19-20",
  grepl(x = Date, pattern = "20-0") ~ "19-20",
  grepl(x = Date, pattern = "20-1") ~ "20-21",
  grepl(x = Date, pattern = "21-0") ~ "20-21",
  grepl(x = Date, pattern = "21-1") ~ "21-22",
  grepl(x = Date, pattern = "22-0") ~ "21-22",
  grepl(x = Date, pattern = "22-1") ~ "22-23",
  grepl(x = Date, pattern = "23-0") ~ "22-23"
))

#Data summary for multiple seasons
site_means <- sites %>% group_by(Name) %>% summarise(mean_ppt = mean(ppt..inches.),
                                                     mean_temp = mean(tmean..degrees.F.),
                                                     mean_maxvpd = mean(vpdmax..hPa.),
                                                     mean_dewpoint = mean(tdmean..degrees.F.)) %>% arrange(Name)

View(site_means)

#Get total seasonal precip per site
precip_totals <- sites %>% group_by(Season) %>% summarise(yearly_total = sum(ppt..inches.))
View(precip_totals)

#get the average total precip per site across seasons
precip_total_means_per_site <- precip_totals %>% group_by(Name) %>% summarise(mean_ppt = mean(yearly_total))
View(precip_total_means_per_site)


#When dealing with one season
site_means <- sites %>% group_by(Name) %>% summarize(mean_ppt = mean(ppt..mm.),
                                                     mean_temp = mean(tmean..degrees.C.),
                                                     mean_maxvpd = mean(vpdmax..hPa.),
                                                     mean_min_temp = mean(tmin..degrees.C.),
                                                     mean_max_temp = mean(tmax..degrees.C.),
                                                     mean_dewpoint = mean(tdmean..degrees.C.))
precip_total_per_site <- sites %>% group_by(Name) %>% summarise(total_ppt = sum(ppt..mm.))




# Visualization -----------------------------------------------------------

mean_site_data <- read.csv("Data/climate/site_climate_data.csv")

#plot each variable together in a matrix
plot(mean_site_data[,3:6])

#get the correlations for each variable
corr <- cor(mean_site_data[,3:6])
corr

#plot the correlations with color and the correlation coefficient
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

#modelling VPD as a function of mean temp and total seasonal precip
fit <- lm(mean_maxvpd_hPA_2013_2023 ~ mean_temp_C_2013_2023 + mean_total_seasonal_ppt_mm_2013_2023, data = mean_site_data)
summary(fit)
