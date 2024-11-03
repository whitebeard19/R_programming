# Load necessary libraries
install.packages("rpart.plot")  # Install if not already installed
library(rpart)
library(rpart.plot)
library(dplyr)

# Load your dataset (replace 'email_data.csv' with your actual file path)
data <- data.frame(
  Category = c("spam", "ham", "spam", "ham", "spam", "ham", "spam", "ham", "spam"),
  Message = c(
    "Congratulations! You've won a $1000 gift card. Click here to claim.",
    "Hey, are we still on for lunch tomorrow?",
    "Limited time offer! Buy now and get 50% off on all items.",
    "Hi Mom, just checking in. I'll call you later.",
    "You have been selected for a free vacation. Reply to confirm.",
    "Let's meet up this weekend and catch up!",
    "Win big prizes in our new sweepstakes! Enter now.",
    "Can you send me the notes from today's meeting?",
    "Earn money fast! Join our program and make $500 per day."
  )
)

# Check for missing values and structure of data
str(data)
summary(data)

# Convert the Category column to a factor (target variable)
data$Category <- as.factor(data$Category)


# Build the decision tree model using the engineered features
model <- rpart(Category ~ Message, data = data, method = "class", control = rpart.control(cp = 0.001, minsplit = 2))

# Plot the decision tree
rpart.plot(model,type=2, extra=2, main = "Decision Tree for Spam Detection")

# Make predictions on the test set
predictions <- predict(model, data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == data$Category) / nrow(data)
cat("Accuracy:",accuracy, "\n")

