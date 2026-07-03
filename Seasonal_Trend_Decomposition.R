# Clear all previous variables
rm(list = ls()) 

# Install 8 million packages again
install.packages('tidyverse')
install.packages('caret')
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

# Import only the relevant csvs
wd = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/Differences/"

# Hourly averages of previous variables, without STDEVs
hourlyFull = read.csv(file=paste0(wd, "Hourly Full.csv"), stringsAsFactors = FALSE)
hourlyClear = read.csv(file=paste0(wd, "Hourly Clear.csv"), stringsAsFactors = FALSE)
hourlyCloudy = read.csv(file=paste0(wd, "Hourly Cloudy.csv"), stringsAsFactors = FALSE)

# Make sure that they are stored as time series
hourlyFull$Time <- dmy_hms(hourlyFull$Time)
hourlyClear$Time <- dmy_hms(hourlyClear$Time)
hourlyCloudy$Time <- dmy_hms(hourlyCloudy$Time)

# Create a new dataframe for for the variable run in each iteration (and for each sky)
hourlyCarbon$Time <- hourlyFull$Time
hourlyCarbon$Radiance <- hourlyFull$Carbon

# Check that the frequency of the time series is... 24? Want 24 obs per day
freqCheck = frequency(hourlyCarbon) 

# Begin to implement the STL
stl(hourlyCarbon, 
    s.window = 7, # Seasonal smoothing parameter (at least 7)
    s.degree = 0,
    t.window = NULL, # Takes the default formula for n(t)
    t.degree = 1, 
    # This is the value to be varied to effectively get what the imposed seasonality will be 
    l.window = if(freqCheck == 24) nextdd(freqCheck) else 25, 
    l.degree = t.degree,
    # Parameters to speed up or slow down the code
    s.jump = ceiling(s.window/10),
    t.jump = ceiling(t.window/10),
    l.jump = ceiling(l.window/10),
    # Toggle TRUE if the data shows significant outliers
    robust = FALSE,
    # Number of loops to get convergence of smoothing
    inner = if(robust)  1 else 2,
    # Number of loops to to eliminate contributions of outliers
    outer = if(robust) 15 else 0,
    # Action to be performed in the event of NA values (of which there are many)
    na.action = na.omit(hourlyCarbon))
