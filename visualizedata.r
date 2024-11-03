# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(ggmap)

# Define the air quality dataset
data <- data.frame(
  Ozone = c(41, 36, 12, 18, NA, 28),
  SolarR = c(190, 118, 149, 313, NA, NA),
  Wind = c(7.4, 8.0, 12.6, 11.5, 14.3, 14.9),
  Temp = c(67, 72, 74, 62, 56, 66),
  Month = c(5, 5, 5, 5, 5, 5),
  Day = c(1, 2, 3, 4, 5, 6)
)



# A) Ozone Concentration Barplot

barplot(data$Ozone, 
        main = "Ozone Concentration", 
        xlab = "Days", 
        ylab = "Ozone", 
        names.arg = data$Day,
        col = "blue")

# B) Maximum Daily Temperature Histogram
hist(data$Temp, 
     main = "Maximum Daily Temperature", 
     xlab = "Temperature (°F)", 
     col = "lightgreen",
     border = "black")


# # C1) Average Wind Speed Boxplot && C2) Multiple Boxplots for All Air Quality Parameters
# Boxplot for Wind
boxplot(data$Wind, 
        main = "Average Wind Speed", 
        ylab = "Wind Speed (mph)", 
        col = "orange")

# Multiple Boxplots for All Parameters
boxplot(data$Ozone, data$SolarR, data$Wind, data$Temp, 
        names = c("Ozone", "Solar Radiation", "Wind", "Temperature"), 
        main = "Boxplots of Air Quality Parameters", 
        ylab = "Values", 
        col = c("lightblue", "lightgreen", "lightyellow", "lightcoral"))



# # D) Ozone Concentration Scatterplot
plot(data$Month, data$Ozone, 
     main = "Ozone Concentration per Month", 
     xlab = "Month", 
     ylab = "Ozone Concentration", 
     col = "purple", 
     pch = 19)


# E) Geographical Map of World Cities
# Install packages if you haven't already
install.packages("maps")
library(maps)

cities <- data.frame(
  City = c("Chennai", "Bengaluru"),
  Latitude = c(13.0827, 12.9716),
  Longitude = c(80.2707, 77.5946)
)

# Basic World Map
map("world", fill = TRUE, col = "lightgrey", bg = "white", lwd = 0.5)
title("Geographical Map of World Cities")

# Add points for each city
points(cities$Longitude, cities$Latitude, col = "blue", pch = 19, cex = 1.5)

# Add text labels for each city
text(cities$Longitude, cities$Latitude, labels = cities$City, pos = 3, cex = 0.8, col = "red")