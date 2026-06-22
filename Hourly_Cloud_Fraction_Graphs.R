# Clear the program
rm(list = ls()) 

# Get the required libraries
library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
# library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) # ?

# Set the working directory for the cloud data
wd <- "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/"

# Get the cloud data as a dataframe (thanks Claude...)
cloud_data_burnside = read.csv(file=paste0(wd, "Hourly Clouds Burnside.csv"), stringsAsFactors = FALSE)
cloud_data_burnside$Time <- dmy_hms(cloud_data_burnside$Time)
cloud_data_burnside <- rename(cloud_data_burnside, Cloud_Fraction = Var1)

cloud_data_gault = read.csv(file=paste0(wd, "Hourly Clouds Gault.csv"), stringsAsFactors = FALSE)
cloud_data_gault$Time <- dmy_hms(cloud_data_gault$Time)
rename(cloud_data_gault, Cloud_Fraction = Var1)

# Mutate the Burnside Data a bit
cloud_data_burnside <- cloud_data_burnside %>% mutate(year = year(Time),
                    month = month(Time, label=TRUE),
                    day = day(Time), 
                    hour = hour(Time))

# Filter for the summer
summer_burnside <- cloud_data_burnside %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

table(summer_burnside$month)

# Begin the plot
p <-ggplot(summer_burnside,aes(day, hour, fill=Cloud_Fraction))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_gradient2(name="Hourly Cloud Fraction", 
                       low = "#96B5DF", 
                       mid = "#C9D0D9", 
                       high = "#989CA0", 
                       midpoint = 0.5, 
                       na.value = 'red')
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(summer_burnside$hour))
p <- p + facet_grid(~ month)
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Cloud Fraction at 217.33K Threshold"), x="Day", y="Hour")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid() #ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness

