# Set seed for reproducibility
set.seed(123)

# Generate synthetic data for 20 employees with YearsExperience, Happiness, and Income
data <- data.frame(
  YearsExperience = round(runif(20, 1, 20), 1),  # Experience in years between 1 and 20
  Happiness = round(runif(20, 1, 10), 1)         # Happiness level between 1 and 10
)

# Generate Income based on YearsExperience with some random noise
data$Income <- 25000 + data$YearsExperience * 3000 + rnorm(20, mean = 0, sd = 5000)

# Linear Model for Income vs Years of Experience
model_income_experience <- lm(Income ~ YearsExperience, data = data)
summary(model_income_experience)

# Linear Model for Income vs Happiness
model_income_happiness <- lm(Income ~ Happiness, data = data)
summary(model_income_happiness)

# Print the data
print(data)

# Plot Income vs Years of Experience with regression line
plot(data$YearsExperience, data$Income, 
     main = "Income vs Years of Experience",
     xlab = "Years of Experience", 
     ylab = "Income",
     pch = 16, col = "blue")
abline(model_income_experience, col = "red", lwd = 2)

# Plot Income vs Happiness with regression line
plot(data$Happiness, data$Income, 
     main = "Income vs Happiness",
     xlab = "Happiness Level", 
     ylab = "Income",
     pch = 16, col = "green")
abline(model_income_happiness, col = "purple", lwd = 2)

# Print R-squared values for each model
cat("R-squared for Income vs Years of Experience:", round(summary(model_income_experience)$r.squared, 4), "\n")
cat("R-squared for Income vs Happiness:", round(summary(model_income_happiness)$r.squared, 4), "\n")
