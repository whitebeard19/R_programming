# Load necessary libraries
library(caret)
library(dplyr)
library(pROC)

# Load the Titanic dataset
# Uncomment and adjust if loading from a file
data <- read.csv("C:\\Projects\\R_Programs\\titanic.csv")



# Check the structure of the data
str(data)

# Convert 'Survived', 'Pclass', and 'Sex' to factors
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)

# Select only the relevant columns
data <- data[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch")]

# Drop rows with any remaining missing values
data <- na.omit(data)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$Survived, p = 0.5, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the Logistic Regression Model
model <- glm(Survived ~ ., data = train_data, family = binomial)

# Predictions on the test data
predictions <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)  # Threshold of 0.5 for classification

# Convert Survived to numeric for comparisons
actual_class <- as.numeric(as.character(test_data$Survived))

# Accuracy Calculation
conf_matrix <- table(actual_class, predicted_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Output the requested details
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Length of Predictions:", length(predicted_class), "\n")

# Count missing values in actual and predicted data
missing_actual <- sum(is.na(actual_class))
missing_predictions <- sum(is.na(predicted_class))
cat("Missing values in Actual:", missing_actual, "\n")
cat("Missing values in Predictions:", missing_predictions, "\n")

# Distribution of actual values
cat("Distribution of Actual Values:\n")
print(table(actual_class))

# Distribution of predicted values
cat("Distribution of Predicted Values:\n")
print(table(predicted_class))

# AUC-ROC for further evaluation
roc_obj <- roc(actual_class, predictions)
auc <- auc(roc_obj)
cat("AUC:", round(auc, 4), "\n")

# Plot ROC Curve
plot(roc_obj, main="ROC Curve for Titanic Survival Prediction", col="blue")
