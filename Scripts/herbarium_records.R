#Poppy Northing
#last edited: 6SEP2023
#Playing around w/ herbarium data

library(tidyverse)

# Occurrences -------------------------------------------------------------
#Really gives you all the data that you would want...
herb_occs <- read.delim("all_pere_occs_13feb2025/occurrences.csv",sep = ",")
herb_occs <- arrange(herb_occs, year)
herb_occs <- filter(herb_occs, basisOfRecord == "PreservedSpecimen") %>% filter(!is.na(year))
View(herb_occs)

herb_occs %>% group_by(year) %>%
  mutate(counts = n()) %>%
  ggplot(aes(x = as.factor(year), y = counts)) +
  geom_point() +
  theme_classic() +
  geom_abline(intercept = 15, slope = 0, color = "red", linetype = "dashed") +
  annotate("text", x = 12, y = 12, label = paste("Total Specimens:", nrow(herb_occs)), size = 4) +
  xlab("Year") + ylab("Number of Specimens") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = 1:160)

herb_occs %>% group_by(year) %>% mutate(counts = n()) %>% filter(counts > 140) %>%
  ggplot(aes(x = as.factor(year))) + geom_bar() + theme_classic()

# Identifications ---------------------------------------------------------
#Identifications: gives date ~identified~ + name of id-er
herb_ids <- read.csv("identifications.csv")
herb_ids <- arrange(herb_ids, dateIdentified)
View(herb_ids)

# Material Sample ---------------------------------------------------------
#Empty in this case, but seems like it gives info on how the samples were prepared?
herb_mats <- read.csv("materialSample.csv")
View(herb_mats)

# Measurement or Fact -----------------------------------------------------
#the only unique data it gives is phenology (ie reproductive stage)
herb_meas <- read.csv("measurementOrFact.csv")
View(herb_meas)
