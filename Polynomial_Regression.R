# Use polynomial regression to fit the data!
# Clear using this!
rm(list = ls()) 

# Install relevant packages
install.packages('tidyverse')
install.packages('caret')
library('tidyverse')
library('caret')
library('ggplot2')

# Load the dataset you need (cloud)
wd = "~/Library/CloudStorage/OneDrive-McGillUniversity/Undergrad/Lab Work/2026 Summer Research/Data/8 Minute CSVs/Cloud Classified Data/"

radiances = read.csv(file=paste0(wd, "Burnside Brightness.csv"), stringsAsFactors = FALSE)

# Randomly split the data for training purposes
set.seed(123)
trainIndex <- createDataPartition(radiances$Count, p = 0.8, list = FALSE)
train.data <- radiances[trainIndex, ]
test.data <- radiances[-trainIndex, ]

# Build the regression model
model6 <- lm(Count ~ poly(Temperatures, 6, raw = TRUE), data = train.data)
# poly(): Generates orthogonal polynomials when raw = FALSE, raw powers when TRUE.
# lm(): Fits linear and polynomial regression models.

# Generate predicted values
pred6 <- predict(model6, test.data)

# Get performance of the model
postResample(pred6, test.data$Count)

# Get a summary of the model
summary(model6)

# Find the minima of the plot

# 1. Create a fine temperature grid over your data range, in other words, divide
# the range into 10 000 small intervals
temp_seq <- seq(min(train.data$Temperatures, na.rm = TRUE),
                max(train.data$Temperatures, na.rm = TRUE),
                length.out = 10000)

# 2. Predict Count over that grid, in other words, change the range from 
# temperatures in my data to a smooth distribution of temperatures, then 
# predict the count values from that number
pred_seq <- predict(model6, newdata = data.frame(Temperatures = temp_seq))

# 3. Compute numerical derivative (slope between consecutive points)
deriv <- diff(pred_seq) / diff(temp_seq)

# 4. Find where derivative changes sign from negative to positive (= local minimum)
sign_changes <- which(diff(sign(deriv)) == 2)  # +2 means neg → pos = minimum

# 5. Report the temperature and predicted Count at each minimum
minima <- data.frame(
  Temperature = temp_seq[sign_changes],
  Predicted_Count = pred_seq[sign_changes]
)
print(minima)

###############################################################################
# Plot the model
# stat_smooth adds a smoothed conditional mean to the curve
ggplot(train.data, aes(Temperatures, Count)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = lm, formula = y ~ poly(x, 6, raw = TRUE), color = "#780073") +
  geom_point(data = minima, aes(x = Temperature, y = Predicted_Count),
             color = "gold", size = 4, shape = 18) +
  geom_label(data = minima, aes(x = Temperature, y = Predicted_Count,
                                label = paste0("Min\n(", round(Temperature, 2), ", ", round(Predicted_Count, 2), ")")),
             nudge_y = -80, size = 3) +
  labs(title="Polynomial Regression of Brightness Temperature Distribution") +
  theme_bw() 
  
