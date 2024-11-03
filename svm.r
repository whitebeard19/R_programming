# Load necessary libraries
# Uncomment to install if not already installed
# install.packages("caret")
# install.packages("e1071")

library(caret)    # For train-test split and model training
library(e1071)    # For SVM

# Load the dataset (replace the file path with the correct path)
heart_df <- data.frame(
    A = c(63,65,67,37,41,56,62,57,63,53,37,56),
    B = c(1,1,0,0,1,0,1,0,1,1,0,1),
    C = c(1,4,4,3,4,1,4,1,4,0,4,3),
    D = c(145,160,120,125,180,130,120,140,140,160,170,150),
    E = c(223,169,228,250,204,288,358,192,158,198,158,151),
    F = c(1,0,1,1,0,1,1,1,1,0,0,1),
    G = c(2,2,2,0,1,1,2,0,0,1,0,1),
    H = c(0,0,0,0,1,1,0,0,1,1,0,0),
    I = c(2.3,1.5,1.6,1.8,2.3,2.5,3.9,3.8,1.5,1.6,2.3,1.8)
)

# Inspect dataset structure and first few rows
str(heart_df)
head(heart_df)


# Set seed for reproducibility and create train-test split (70% train, 30% test)
set.seed(3003)
train_indices <- createDataPartition(heart_df$F, p = 0.5, list = FALSE)
training <- heart_df[train_indices, ]
testing <- heart_df[-train_indices, ]

# Check dimensions and for missing values
print(dim(training))
print(dim(testing))
cat("Missing values in dataset:", any(is.na(heart_df)), "\n")

# Convert 'F' variable to a factor for classification
training$F <- as.factor(training$F)
testing$F <- as.factor(testing$F)

# Train SVM model with linear kernel using repeated cross-validation
set.seed(3233)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_linear <- train(F ~ ., data = training,
                    method = "svmLinear",
                    trControl = train_control,
                    preProcess = c("center", "scale"),
                    tuneGrid = expand.grid(C = seq(0.1, 5, by = 0.1))  # Tuning parameter 'C'
)

# Print model results and plot accuracy vs cost
print(svm_linear)

# Extract the results for plotting
svm_results <- svm_linear$results

# Plot cost vs accuracy using base R plot function
plot(svm_results$C, svm_results$Accuracy, type = "b",
     col = "blue", pch = 19, xlab = "Cost (C)",
     ylab = "Accuracy", main = "Cost vs Accuracy for SVM Linear Model")
grid()  # Add a grid for better readability


# Make predictions on the test set
test_pred <- predict(svm_linear, newdata = testing)

# Create and print confusion matrix to evaluate model performance
conf_matrix <- confusionMatrix(test_pred, testing$F)
print(conf_matrix)
