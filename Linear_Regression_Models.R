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
hourlyClear <- hourlyClear %>% mutate(year = year(Time),
                                month = month(Time, label=TRUE),
                                day = day(Time), 
                                hour = hour(Time))

metGault <- metGault %>% mutate(year = year(Time),
                                month = month(Time, label=TRUE),
                                day = day(Time), 
                                hour = hour(Time))

# Filter for months
hourlyClear <- hourlyClear %>%
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

################################# Met Data Plots ###############################

# Create plots in the grid idiom for each met variable and arrage them

metGault$Wind <- replace(metGault$Wind, which(metGault$Wind > 50), NA)
metGault$MeanTemp <- replace(metGault$MeanTemp, which(metGault$MeanTemp < 0), NA)

wind <-ggplot(metGault,aes(day, hour, fill=Wind))+
  geom_tile(color= 'white',size=0.1) + 
  scale_fill_gradient2(name="Mean Wind", 
                       low = 'white', 
                       mid = "lightblue", 
                       high = 'darkblue', 
                       midpoint = 0.5, 
                       na.value = 'black')
wind <- wind + scale_y_continuous(trans = "reverse", breaks = unique(metGault$hour))
wind <- wind + facet_grid(~ month)
wind <- wind + scale_x_continuous(breaks =c(1,10,20,31))
wind <- wind + theme_minimal(base_size = 5)
wind <- wind + labs(title= paste("Mean Wind"), x="Day", y="Hour")
wind <- wind + theme(legend.position = "bottom")+
  #theme(plot.title=element_text(size = 6))+
  theme(axis.text.y=element_text(size=4)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=6))+
  theme(legend.title=element_text(size=4))+
  theme(legend.text=element_text(size=4))+
  removeGrid() #ggExtra

temperature <-ggplot(metGault,aes(day, hour, fill=MeanTemp))+
  geom_tile(color= 'white',size=0.1) + 
  scale_fill_gradient2(name="Mean Temperature", 
                       low = 'white', 
                       #mid = "red", 
                       high = 'darkred', 
                       #midpoint = 0.5, 
                       na.value = 'black')
temperature <- temperature + scale_y_continuous(trans = "reverse", breaks = unique(metGault$hour))
temperature <- temperature + facet_grid(~ month)
temperature <- temperature + scale_x_continuous(breaks =c(1,10,20,31))
temperature <- temperature + theme_minimal(base_size = 5)
temperature <- temperature + labs(title= paste("Mean Temperature"), x="Day", y="Hour")
temperature <- temperature + theme(legend.position = "bottom")+
  #theme(plot.title=element_text(size = 6))+
  theme(axis.text.y=element_text(size=4)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=6))+
  theme(legend.title=element_text(size=4))+
  theme(legend.text=element_text(size=4))+
  removeGrid() #ggExtra

humidity <-ggplot(metGault,aes(day, hour, fill=MeanRH))+
  geom_tile(color= 'white',size=0.1) + 
  scale_fill_gradient2(name="Mean Relative Humidity", 
                       low = 'white', 
                       #mid = '', 
                       high = 'darkgreen', 
                       #midpoint = 0.5, 
                       na.value = 'black')
humidity <- humidity + scale_y_continuous(trans = "reverse", breaks = unique(metGault$hour))
humidity <- humidity + facet_grid(~ month)
humidity <- humidity + scale_x_continuous(breaks =c(1,10,20,31))
humidity <- humidity + theme_minimal(base_size = 5)
humidity <- humidity + labs(title= paste("Mean Relative Humidity"), x="Day", y="Hour")
humidity <- humidity + theme(legend.position = "bottom")+
  #theme(plot.title=element_text(size = 6))+
  theme(axis.text.y=element_text(size=4)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=6))+
  theme(legend.title=element_text(size=4))+
  theme(legend.text=element_text(size=4))+
  removeGrid() #ggExtra

# Arrange into full grid using ggarrange
metGrid <- plot_grid(wind, temperature, humidity, nrow = 3, ncol = 1)
  
metGrid

################################ Linear Regressions ############################

# Make a dataframe with only the values of a particular band and cloud type

chosenBand <- metGault
chosenBand$Diff <- hourlyClear$Carbon
chosenBand <- chosenBand %>% relocate(Diff, .before = Wind)

# chosenBand without time and maximum wind and hour day etc.
chosenBand$Time <- NULL
chosenBand$WindMax <- NULL
chosenBand$month <- NULL
chosenBand$year <- NULL
chosenBand$day <- NULL
chosenBand$hour <- NULL

# Convert chosenBand to a time series object

chosenBandTS <- ts(chosenBand, 
              start = start(metGault$Time),
              end = end(metGault$Time),
              frequency = frequency(metGault$Time))

# Run a linear regression model for one variable
regression <- tslm(
  Diff ~ Wind + MeanTemp + MeanRH,
  data=chosenBandTS)

# Get summary statistics of the model
summary(regression)

# Check the residuals of the model
checkresiduals(regression)

# Matrix representation
regressionMatrix <- chosenBandTS %>%
  as.data.frame() %>%
  GGally::ggpairs()

regressionMatrix

# Fitted data against data (predictive capacity)
predictions <- autoplot(chosenBandTS[,'Diff'], series="Experimental Data") +
  autolayer(fitted(regression), series="Fitted Linear Model") +
  xlab("Hour") + 
  ylab("Radiance Difference") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

predictions

# Residuals against data (to check)
chosenBand[,"Residuals"]  <- as.numeric(residuals(regression))
p1 <- ggplot(chosenBand, aes(x=Wind, y=Residuals)) +
  geom_point()
p2 <- ggplot(chosenBand, aes(x=MeanTemp, y=Residuals)) +
  geom_point()
p3 <- ggplot(chosenBand, aes(x=MeanRH, y=Residuals)) +
  geom_point()

gridExtra::grid.arrange(p1, p2, p3, ncol=3)

######################### ARIMA Linear Regressions ############################

arimaRegresion <- auto.arima(chosenBand[, "Diff"], xreg = chosenBand[, "MeanTemp"])
summary(arimaRegresion)
checkresiduals(arimaRegresion)

arimaRegresion
