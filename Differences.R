# Plot the differences between Gault and Burnside Data, and find their significances

# clear the workspace
rm(list = ls()) 

# Install relevant packages
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


# Import the relevant files and convert into times
wd = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/Differences/"

# Mean values at each wavenumber for each sky type
tableMeans = read.csv(file=paste0(wd, "All Sites Means.csv"), stringsAsFactors = FALSE)
tableSTD = read.csv(file=paste0(wd, "All Sites STD.csv"), stringsAsFactors = FALSE)

# Wavenumber significances (significance and p values)
wavenumberSignificance = read.csv(file=paste0(wd, 'Wavenumber Significance.csv'), stringsAsFactors = FALSE)

# Hourly averages of previous variables, without STDEVs
hourlyFull = read.csv(file=paste0(wd, "Hourly Full.csv"), stringsAsFactors = FALSE)
hourlyClear = read.csv(file=paste0(wd, "Hourly Clear.csv"), stringsAsFactors = FALSE)
hourlyCloudy = read.csv(file=paste0(wd, "Hourly Cloudy.csv"), stringsAsFactors = FALSE)

# Mean differences for each wavenumber
wavenumberDiffMeans = read.csv(file=paste0(wd, "Wavenumber Difference Means.csv"), stringsAsFactors = FALSE)
wavenumberDiffSTD = read.csv(file=paste0(wd, "Wavenumber Difference STD.csv"), stringsAsFactors = FALSE)

# Time series for each wavenumber band
TTFull = read.csv(file=paste0(wd, "Time Table Full.csv"), stringsAsFactors = FALSE)
TTFullSTD = read.csv(file=paste0(wd, "Time Table Full STD.csv"), stringsAsFactors = FALSE)
TTClear = read.csv(file=paste0(wd, "Time Table Clear.csv"), stringsAsFactors = FALSE)
TTClearSTD = read.csv(file=paste0(wd, "Time Table Clear STD.csv"), stringsAsFactors = FALSE)
TTCloudy = read.csv(file=paste0(wd, "Time Table Cloudy.csv"), stringsAsFactors = FALSE)
TTCloudySTD = read.csv(file=paste0(wd, "Time Table Cloudy STD.csv"), stringsAsFactors = FALSE)

# Make sure that Time Series have the right variables

TTFull$Time <- dmy_hms(TTFull$Time)
TTFullSTD$Time <- dmy_hms(TTFullSTD$Time)
TTClear$Time <- dmy_hms(TTClear$Time)
TTClearSTD$Time <- dmy_hms(TTClearSTD$Time)
TTCloudy$Time <- dmy_hms(TTCloudy$Time)
TTCloudySTD$Time <- dmy_hms(TTCloudySTD$Time)

hourlyFull$Time <- dmy_hms(hourlyFull$Time)
hourlyClear$Time <- dmy_hms(hourlyClear$Time)
hourlyCloudy$Time <- dmy_hms(hourlyCloudy$Time)


########################### Hourly Heatmaps ##############################

# Mutate to be able to plot!

hourlyFull <- hourlyFull %>% mutate(year = year(Time),
                                month = month(Time, label=TRUE),
                                day = day(Time), 
                                hour = hour(Time))

hourlyClear <- hourlyClear %>% mutate(year = year(Time),
                                    month = month(Time, label=TRUE),
                                    day = day(Time), 
                                    hour = hour(Time))

hourlyCloudy <- hourlyCloudy %>% mutate(year = year(Time),
                                    month = month(Time, label=TRUE),
                                    day = day(Time), 
                                    hour = hour(Time))

# Now filter through to the months that you want!
hourlyFull <- hourlyFull %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

hourlyClear <- hourlyClear %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

hourlyCloudy <- hourlyCloudy %>%
  filter(month %in% c("Jun", "Jul", "Aug"), 
         year == 2024) %>%
  mutate(month = droplevels(month))

# Plot the hourly values in the way of Cloud Fraction

# Remake a graphs like the cloud cover one, but with hourly grouped values
p <-ggplot(hourlyFull,aes(day, hour, fill=Methane))+
  geom_tile(color= 'black',size=0.1) + 
  scale_fill_gradient2(name="Ozone Hourly Differences", 
                       low = 'darkblue', 
                       mid = "white", 
                       high = "darkred", 
                       midpoint = 0.5, 
                       na.value = 'black')
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(hourlyFull$hour))
p <- p + facet_grid(~ month)
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Ozone Radiance Differences"), x="Day", y="Hour")
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

###################### Composite line plots Burn/Gault ########################

# Next, create a composite line chart with the wavenumber data from three sky types
# and full differences over wavenumber (updated code)

# Data should have [wavenumbers, fullBurn, fullGault, ...]

# Remove bottom row (clouds) for this plot

tableMeans0 <- tableMeans[-nrow(tableMeans), ]

# Transform data into long (tidy) format so that ggplot can plot it!

# Transform your data
tidyMeans <- tableMeans0 %>%
  pivot_longer(
    cols = c(burnsideFull, gaultFull, burnsideClear, gaultClear, burnsideCloudy, gaultCloudy), # The columns you want to combine
    names_to = "Site",                                 # Name for the new category column
    values_to = "Value"                                    # Name for the new data values column
  )

#### MISC ####
font <- "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

caption_text  <- str_glue("**Design:** Luke Martin<br>","**Date:** 06/2026")

# Start on the main plot
p1 <- tidyMeans %>% 
  ggplot() +
  #geom_hline(yintercept = 100,linetype="solid", size=.25) + adds a line at 100
  #geom_point(data=tableMeans %>%  # Adds a point at the end of every line (no need)
               #group_by(row) %>% 
               #slice_max(date),
             #aes(x=date, y=value, color=country),shape=16) +
  geom_line(aes(x=wavenumbers, y=Value, color=Site)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  #geom_text(data=tidyMeans %>% # I think a label at the end of the plot..? not needed
              #group_by(Site) %>% 
#              slice_max(wavenumbers),
 #           aes(x=wavenumbers, y=Value, color=Site, label = round(Value)),
  #          hjust = -.5, vjust = .5, size=2.5, family=font, fontface="bold") +
  scale_color_met_d(name="Redon") +
  # scale_x_conitnuous(breaks = c(600, 800, 1000, 1200)) +
  # scale_y_continuous(breaks = c(0,40,80,120,160),
                     #labels = c("","","100","","") adds a label at 100
  #) +
  #facet_wrap(~ country) +
  facet_wrap(~  factor(Site)) + #, levels=c(burnsideFull, gaultFull, burnsideClear, gaultClear, burnsideCloudy, gaultCloudy))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=7),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )

p1

# Fancy title for this plot 

#text <- tibble(
#  x = 0, y = 0,
#  label = "The consumer confidence indicator provides an indication of future developments of households’ consumption and saving. An indicator above 100 signals a boost in the consumers’ confidence towards the future economic situation. Values below 100 indicate a pessimistic attitude towards future developments in the economy, possibly resulting in a tendency to save more and consume less. During 2022, the consumer confidence indicators have declined in many major economies around the world.<br>"
#)

#sub <- ggplot(text, aes(x = x, y = y)) +
#  geom_textbox(
#    aes(label = label),
#    box.color = bg, fill=bg, width = unit(10, "lines"),
#    family=font, size = 3, lineheight = 1
#  ) +
#  coord_cartesian(expand = FALSE, clip = "off") +
#  theme_void() +
#  theme(plot.background = element_rect(color=bg, fill=bg))


# TITLE
#text2 <- tibble(
#  x = 0, y = 0,
#  label = "**Consumer Confidence Around the World**<br>"
#)

#title <- ggplot(text2, aes(x = x, y = y)) +
#  geom_textbox(
#    aes(label = label),
#    box.color = bg, fill=bg, width = unit(12, "lines"),
#    family=font, size = 10, lineheight = 1
#  ) +
#  coord_cartesian(expand = FALSE, clip = "off") +
#  theme_void() +
#  theme(plot.background = element_rect(color=bg, fill=bg))

#finalPlot <- (title+sub)/p1 +
#  plot_layout(heights = c(1, 2)) +
#  plot_annotation(
#    caption = caption_text,
#    theme=theme(plot.caption = element_markdown(hjust=0, margin=margin(20,0,0,0), size=6, color=txt_col, lineheight = 1.2),
#                plot.margin = margin(20,20,20,20),))


####################### Difference Plots w/ Uncertainties ######################

# wavenumberDiffMeans
# wavenumberDiffSTD

# Format: [wavenumber, full sky, clear, cloudy]

means_long <- wavenumberDiffMeans %>%
  pivot_longer(
    cols = c(Full.Sky, Clear, Cloudy), # The columns you want to combine
    names_to = "Sky",                                 # Name for the new category column
    values_to = "Means"                               # Name for the new data values column
  )

sd_long <- wavenumberDiffSTD %>%
  pivot_longer(
    cols = c(Full.Sky, Clear, Cloudy),
    names_to = "Sky",
    values_to = "STD"
  )

df_join <- means_long %>% 
  left_join(sd_long)
#> Joining, by = c("date", "variable")

# Try to associate each group to a specific colour using:
#group.colors = c(A =, B =, C =,), then 

p <- ggplot(data = df_join, aes(x = Wavenumbers, group = Sky)) + 
  geom_line(aes(y = Means, color = Sky), size = 0.1) + 
  geom_ribbon(aes(y = Means, ymin = 0 - STD, ymax = 0 + STD, fill = Sky), alpha = .2) + # Put ribbon at 0, null hypothesis...
  xlab("Wavenumber") + 
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.175,.5), legend.direction = "vertical") +
  theme(legend.title = element_blank()) +
  facet_grid(Sky~.) # Facet into rows based on the sky
  #scale_fill_manual(values=group.colors) # if you want to choose the colours specifically
  
p 

####################### Difference Plots w/ Significances ######################

sigMeans <- wavenumberDiffMeans
sigMeans$Full.Sky <- wavenumberSignificance$sigDiffFull
sigMeans$Clear <- wavenumberSignificance$sigDiffClear
sigMeans$Cloudy <- wavenumberSignificance$sigDiffCloudy

sigMeansLong <- sigMeans %>%
  pivot_longer(
    cols = c(Full.Sky, Clear, Cloudy),
    names_to = "Sky", 
    values_to  = "Sig"
  )

sig_join <- means_long %>% 
  left_join(sigMeansLong)
#> Joining, by = c("date", "variable")
  
p <- ggplot(data = sig_join, aes(x=Wavenumbers, group = Sky)) +
  geom_line(aes(y = Means, color = Sky), size = 0.1) +
  geom_line(aes(y = 0), color = 'black', size = 0.5) +
  geom_ribbon(aes(ymin = (-10 * Sig), ymax = (20 * Sig), fill = Sky), alpha = 0.2) +
  xlab("Wavenumber") +  
  theme_bw() +  
  theme(legend.key = element_blank()) + 
  theme(plot.margin=unit(c(1,3,1,1),"cm"))+
  theme(legend.position = c(1.175,.5), legend.direction = "vertical") +
  theme(legend.title = element_blank()) +
  facet_grid(Sky~.)

p