# Install and load required libraries
library(ggplot2)
library(cluster)
library(factoextra)

# Create the dataset
mall_data <- data.frame(
    Customers = c(1, 2, 3, 4, 5, 6, 7),
    Gender = c("male", "male", "female", "female", "female","male","female"),
    Age = c(19, 21, 20, 23, 31, 27, 45),
    Annual_Income = c(15, 15, 16, 16, 17, 18, 20),
    Spending_Score = c(38, 81, 6, 77, 40, 99, 15)
)

# Select relevant features for clustering
mall_data_selected <- mall_data[, c("Annual_Income", "Spending_Score")]
# print(mall_data_selected)

# Run K-means clustering with 2 clusters
set.seed(123)
kmeans_result <- kmeans(mall_data_selected, centers = 2)

# Add the cluster labels to the original dataset
mall_data$Cluster <- as.factor(kmeans_result$cluster)

# View the dataset with cluster assignments
print(mall_data)

# Print cluster centers
print("Cluster Centers:")
print(kmeans_result$centers)

# Plot the clusters using base R plot
plot(mall_data_selected, col = kmeans_result$cluster, pch = 19,
     xlab = "Annual Income", ylab = "Spending Score",
     main = "K-means Clustering of Mall Customers")

# Add cluster centers
points(kmeans_result$centers, col = 1:2, pch = 8, cex = 2)

# Annotate each data point with the customer number
text(mall_data_selected$Annual_Income, mall_data_selected$Spending_Score, 
     labels = mall_data$Customers, pos = 3, cex = 0.8, col = "black")

