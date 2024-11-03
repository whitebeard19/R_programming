# Load necessary libraries
library(rpart)
library(rpart.plot)

# Define the dataset
data <- data.frame(
  Day = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14"),
  Outlook = c("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast", "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain"),
  Temp = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  Play = c("No", "No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Convert relevant columns to factors
data$Outlook <- as.factor(data$Outlook)
data$Temp <- as.factor(data$Temp)
data$Humidity <- as.factor(data$Humidity)
data$Wind <- as.factor(data$Wind)
data$Play <- as.factor(data$Play)

table(data$Play)

# Build the decision tree model
model <- rpart(Play ~ Outlook + Temp + Humidity + Wind, data = data, method = "class", control = rpart.control(cp = 0.001, minsplit = 3))

# Plot the decision tree
rpart.plot(model,type = 2, extra = 2, main = "Decision Tree for Tennis Play Decision")

# Make predictions on the dataset
predictions <- predict(model, data, type = "class")

# Display predictions
print("Predictions:")
print(predictions)

# Calculate accuracy
accuracy <- sum(predictions == data$Play) / nrow(data)
cat("Accuracy:", accuracy, "\n")

# Distribution of actual vs predicted values
# cat("Distribution of actual values:\n")
# print(table(data$Play))
# cat("Distribution of predicted values:\n")
# print(table(predictions))
# 