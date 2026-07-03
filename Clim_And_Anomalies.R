# Clear all previous variables
rm(list = ls()) 

# Install 8 million packages again
install.packages('tidyverse')
install.packages('caret')
install.packages('GGally')
install.packages("ggpubr")
install.packages('forecast')
library(cowplot)
library('tidyverse')
library(janitor)
library('caret')
library(showtext)
library('ggplot2')
library('dplyr')
library('lubridate')
library('ggExtra')
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(forecast)
library(ggpubr)

# Import only the relevant csvs
wdDiff = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/Differences/"
wdMet = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/Meteorological/"

# Hourly averages of all variables, without STDEVs
hourlyFull = read.csv(file=paste0(wdDiff, "Hourly Full.csv"), stringsAsFactors = FALSE)
hourlyClear = read.csv(file=paste0(wdDiff, "Hourly Clear.csv"), stringsAsFactors = FALSE)
hourlyCloudy = read.csv(file=paste0(wdDiff, "Hourly Cloudy.csv"), stringsAsFactors = FALSE)

# Meteorological Data
metGault = read.csv(file=paste0(wdMet, "Gault Met Values.csv"), stringsAsFactors = FALSE)
metGaultQC = read.csv(file=paste0(wdMet, "Gault Met QC.csv"), stringsAsFactors = FALSE)

# Make sure that they are stored as time series
hourlyFull$Time <- dmy_hms(hourlyFull$Time)
hourlyClear$Time <- dmy_hms(hourlyClear$Time)
hourlyCloudy$Time <- dmy_hms(hourlyCloudy$Time)
metGault$Time <- dmy_hms(metGault$Time)
metGaultQC$Time <- dmy_hms(metGault$Time)

# Eventually, should have a line which take the difference between Burnside and Gault to regress against the other difference!
# Data should be stationary

# Get month,  day, and hour from the data
hourlyCloudy <- hourlyCloudy %>% mutate(year = year(Time),
                                      month = month(Time, label=TRUE),
                                      day = day(Time), 
                                      hour = hour(Time))

metGault <- metGault %>% mutate(year = year(Time),
                                month = month(Time, label=TRUE),
                                day = day(Time), 
                                hour = hour(Time))

# Filter for months
hourlyCloudy <- hourlyCloudy %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

metGault <- metGault %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

# Remove outlying values

metGault$Wind <- replace(metGault$Wind, which(metGault$Wind > 50), NA)
metGault$MeanTemp <- replace(metGault$MeanTemp, which(metGault$MeanTemp < 0), NA)

########################## Take Hourly Climatology #############################

climDLRClear <- hourlyCloudy %>% 
  group_by(hour) %>%
  summarise(across(c(Time, Total, Carbon, Ozone, AtmosphericWindow, Methane, Clouds), mean, na.rm = TRUE))

climMet <- metGault %>% 
  group_by(hour) %>%
  summarise(across(c(Time, MeanTemp, Wind, MeanRH), mean, na.rm = TRUE))

########################## Basic Hourly Clim Plots #############################

# Make data tidy so that can group by band!

tidyClimDLR <- climDLRClear %>%
  pivot_longer(
    cols = c(Total, Carbon, Ozone, AtmosphericWindow, Methane), 
    names_to = "Band",
    values_to = "Differences"
  )

# Hours are currently starting at noon rather than midnight, subtract 12 from hour!
tidyClimDLR$hour = (tidyClimDLR$hour - 12) %% 24 # Modulo operator wraps negative values!

# Make a plot with gghighlight grouped by Band
p <- ggplot(data = tidyClimDLR, aes(x = hour, group = Band)) + 
  geom_line(aes(y=0), color = 'black', size = 0.2) +
  geom_line(aes(y = Differences, color = Band), size = 0.5) + 
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(colour = alpha("grey85", 1))) +
  scale_color_viridis_d() +
  xlab("Hour") + 
  theme_minimal() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.175,.5), legend.direction = "vertical") +
  theme(legend.title = element_blank()) +
  facet_wrap(~Band)

p

############################# Anomaly Critereon ################################

# Get the standard deviations of each column
overall_sds <- hourlyCloudy %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))

# Get the standard errors of each column
overall_ses <- hourlyCloudy %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))))

# Convert to z-scores
z_scores <- hourlyCloudy %>%
  group_by(hour) %>%
  mutate(across(
    where(is.numeric),
    ~ (.x - mean(.x, na.rm = TRUE)) / overall_ses[[cur_column()]],
    .names = "{.col}_anomaly"
  )) %>%
  ungroup() %>%
  mutate(across(
    ends_with("_anomaly"),
    ~ abs(.x) > 3,
    .names = "{.col}_flag"
  )) %>%
  select(Time, hour, ends_with("_anomaly"), ends_with("_flag"))
  
# Make new objects for the clean data

tidyChosenSky <- hourlyCloudy %>%
  pivot_longer(
  cols = c(Total, Carbon, Ozone, AtmosphericWindow, Methane), 
  names_to = "Band",
  values_to = "Differences"
  )

tidyFlags <- z_scores %>%
  pivot_longer(
    cols = c(Total_anomaly_flag, Carbon_anomaly_flag, Ozone_anomaly_flag, AtmosphericWindow_anomaly_flag, Methane_anomaly_flag), 
    names_to = "Band",
    values_to = "Anomaly"
  )

# Add Differences column to tidyFlags
tidyFlags$Differences <- tidyChosenSky$Differences

tidyAnom <- tidyFlags %>% 
  filter(Anomaly == TRUE) %>%
  mutate(Band = sub("_anomaly_flag$", "", Band))

# Make a plot overlaying anomalous values

pSky <- ggplot(data = tidyChosenSky, aes(x = Time, group = Band)) + 
  geom_line(aes(y=0), color = 'black', size = 0.2) +
  geom_line(aes(y = Differences, color = Band), size = 0.5) + 
  gghighlight(use_direct_label = FALSE, unhighlighted_params = list(colour = alpha("grey85", 1))) +
  geom_rect(data = tidyAnom, aes(xmin = Time - minutes(30), xmax = Time + minutes(30), ymin = - 30, ymax = 30, fill = Band), 
            size = 0.001, alpha = 0.1) +
  scale_color_viridis_d() +
  xlab("Time") + 
  theme_minimal() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.175,.5), legend.direction = "vertical") +
  theme(legend.title = element_blank()) +
  facet_wrap(~Band)

pSky

#################### Regress Anomalies to ENVR Variables #######################




