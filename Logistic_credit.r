# Load necessary libraries
library(caret)
library(pROC)

# Step 1: Load the dataset
# Assuming the dataset is stored as 'credit_data.csv'
data <- read.csv("C:\\Projects\\R_Programs\\credit_data.csv")

# Step 2: Exploratory Data Analysis
str(data)   # Check the structure
summary(data)  # Summary statistics
table(data$Default)  # Check target class distribution

# Step 3: Data Cleaning (if needed)
# Remove any missing values
data <- na.omit(data)

# Step 4: Train-Test Split
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$Default, p = 0.5, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

str(train_data)
str(test_data)

# Step 5: Train the Logistic Regression Model
# Assuming 'Default' is the target variable and rest are predictors
model <- glm(Default ~ ., data = train_data, family = binomial)

# Step 6: Model Summary
summary(model)

# Step 7: Predictions on Test Data
predictions <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)  # Threshold of 0.5

# Step 8: Model Evaluation
# Confusion Matrix
conf_matrix <- table(test_data$Default, predicted_class)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# AUC-ROC Curve
roc_obj <- roc(test_data$Default, predictions)
auc <- auc(roc_obj)
print(paste("AUC:", round(auc, 4)))

# Plot ROC Curve
plot(roc_obj, main="ROC Curve for Logistic Regression", col="red")
