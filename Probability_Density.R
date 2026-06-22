# clear the workspace
rm(list = ls()) 

# Install relevant packages
install.packages('tidyverse')
install.packages('caret')
library('tidyverse')
library('caret')
library('ggplot2')


# Load the dataset you need (cloud)
wd = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/8 Minute CSVs/Cloud Classified Data/"

brightness = read.csv(file=paste0(wd, "Burnside Brightness.csv"), stringsAsFactors = FALSE)

# Make a probability distribution and find minima

# 1. Compute the density estimate 
density_estimate <- density(brightness$Temperatures, na.rm = TRUE)

# 2. Convert to dataframe
dens_df <- data.frame(x = density_estimate$x, y = density_estimate$y)

# 3. Find local minima (where slope changes from negative to positive)
deriv <- diff(dens_df$y) / diff(dens_df$x)
sign_changes <- which(diff(sign(deriv)) == 2)  # neg → pos = minimum

minima <- data.frame(
  Temperature = dens_df$x[sign_changes],
  Density     = dens_df$y[sign_changes]
)
print(minima)

# Basic density
density_plot <- ggplot(brightness, aes(x = Temperatures)) +
  geom_density(colour = "#780073") +
  geom_point(data = minima, aes(x = Temperature, y = Density),
             color = "gold", size = 3, shape = 18) +
  geom_label(data = minima, aes(x = Temperature, y = Density,
                                label = paste0("(", round(Temperature, 2), ")")),
             nudge_y = -0.001, size = 3) +
  labs(title = "Temperature Density Distribution",
       x = "Temperature (°C)", y = "Density") +
  theme_bw()

density_plot
