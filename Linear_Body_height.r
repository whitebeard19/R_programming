# Sample dataset (replace with your own data if available)
data <- data.frame(
  BodyMass = c(50, 60, 70, 80, 90, 100, 110, 120),
  Height = c(150, 155, 160, 165, 170, 175, 180, 185)
)

# Train the Linear Regression Model
model <- lm(Height ~ BodyMass, data = data)

# Display model summary
summary(model)

# Predictions using the model
predictions <- predict(model, newdata = data)

# Plot the data and regression line
plot(data$BodyMass, data$Height, 
     main = "BodyMass vs Height with Regression Line",
     xlab = "Body Mass (kg)", 
     ylab = "Height (cm)", 
     pch = 16, col = "blue")
abline(model, col = "red", lwd = 2) # Add regression line in red

# Print R-squared
cat("R-squared:", round(summary(model)$r.squared, 4), "\n")
