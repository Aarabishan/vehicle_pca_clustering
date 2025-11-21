# Install the necessary packages
install.packages(c("tidyverse", "cluster", "factoextra", "GGally"))
install.packages("e1071")

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(GGally)
library(dplyr)
library(dplyr)
library(ggplot2)
library(e1071)
library(ggrepel)
library(reshape2)

# loading the csv file
vehicles_dataset <- read.csv("C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Question_2/vehicles.csv")

# View structure
str(vehicles_dataset)
summary(vehicles_dataset)

# Check for missing values
colSums(is.na(vehicles_dataset))

# Check for "?" in columns
sapply(vehicles_dataset, function(x) if(is.character(x)) any(x == "?", na.rm = TRUE) else FALSE)

#count total missing values 
sum(is.na(vehicles_dataset))

# Check for "??" in columns
sapply(vehicles_dataset, function(x) if(is.character(x)) any(x == "??", na.rm = TRUE) else FALSE)

# Count "?" values
sum(sapply(vehicles_dataset, function(x) if(is.character(x)) sum(x == "?", na.rm = TRUE) else 0))

# Count "??" values
sum(sapply(vehicles_dataset, function(x) if(is.character(x)) sum(x == "??", na.rm = TRUE) else 0))

#calculate skewness
skew_vals <- sapply(vehicles_dataset[, sapply(vehicles_dataset, is.numeric)], skewness)
print(skew_vals)

# Boxplot for all numeric features to find outliers
vehicles_numeric <- vehicles_dataset %>% select_if(is.numeric)
boxplot(vehicles_numeric, main = "Boxplots of Numeric Features")

#============================Start cleaning process====================================


# Calculate median of each numeric feature grouped by class
median_per_class <- vehicles_dataset %>%
  group_by(class) %>%
  summarise(across(where(is.numeric), ~ median(., na.rm = TRUE)))

# Print the result
print(median_per_class)

# Impute null values in numeric columns using class-wise median
vehicles_imputed <- vehicles_dataset %>%
  group_by(class) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Check if null values are still available
colSums(is.na(vehicles_imputed))

#count total missing values 
sum(is.na(vehicles_imputed))

# Remove the class column temporarily for standardization since it is a categorical feature
vehicle_features <- vehicles_imputed[, -which(names(vehicles_imputed) == "class")]

# Standardize the features
vehicle_scaled <- scale(vehicle_features)

# You can check the result
head(vehicle_scaled)

#=======================Dimensionality reduction using PCA=====================

# Perform PCA
pca_result <- prcomp(vehicle_scaled, center = TRUE, scale. = TRUE)

# View explained variance
summary(pca_result)

#====================================================================================================

# Calculate Proportion of variance Explained (PVE)
pve <- (pca_result$sdev)^2 / sum((pca_result$sdev)^2)

# Calculate cumulative PVE
cum_pve <- cumsum(pve)


#Creating Scree plot and Cumulative PVE plot for all Principle components

# Create a data frame for plotting
pve_df <- data.frame(
  PC = 1:length(pve),
  PVE = pve,
  Cumulative_PVE = cum_pve
)

# Scree Plot: Proportion of Variance Explained
ggplot(pve_df, aes(x = PC, y = PVE)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  xlab("Principal Component") +
  ylab("Proportion of Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


# Cumulative PVE Plot
ggplot(pve_df, aes(x = PC, y = Cumulative_PVE)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  xlab("Principal Component") +
  ylab("Cumulative Proportion of Variance Explained") +
  ggtitle("Cumulative PVE Plot") +
  ylim(0, 1)


# Create a data frame with selected PC ranges and cumulative variance explained
pc_ranges <- c("PC1–PC4", "PC1–PC5", "PC1–PC6", "PC1–PC7", "PC1–PC8")
cum_values <- round(c(cum_pve[4], cum_pve[5], cum_pve[6], cum_pve[7], cum_pve[8]) * 100, 2)

variance_table <- data.frame(
  PCs = pc_ranges,
  `Cumulative Variance Explained (%)` = cum_values
)

# Print the table
print(variance_table, row.names = FALSE)


#Selected only first 5 PCs which covers almost 90% of variance

# Keep only the first 5 principal components
vehicles_pca <- as.data.frame(pca_result$x[, 1:5])

head(vehicles_pca)

#write.csv(vehicles_pca, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Question_2/vehicles_pca.csv" ) 
#=======================================================================================================

#####Below plots are not used in the final report
# Scree plot and Cumulative PVE plot only for first 7 Principle components to get the better understanding on variance

# Plot Scree Plot for first 7 PCs
PVEplot <- qplot(c(1:7), pve[1:7]) +  
  geom_line() +  
  xlab("Principal Component") +  
  ylab("PVE") + 
  ggtitle("Scree Plot (only for First 7 PCs)") + 
  ylim(0, 1)

print(PVEplot)

# Cumulative PVE plot – first 7 PCs
CUMplot <- qplot(1:7, cum_pve[1:7]) +  
  geom_line() +  
  xlab("Principal Component") +  
  ylab("Cumulative PVE") + 
  ggtitle("Cumulative PVE (First 7 PCs)") + 
  ylim(0, 1)

print(CUMplot)


#============================================K-Mean Clustering===============

# Set seed for reproducibility
set.seed(123)  

# Elbow method: WSS for k = 1 to 10
wss <- numeric(10)
for (k in 1:10) {
  kmeans_result <- kmeans(vehicles_pca[, 1:5], centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# Plot the elbow graph
plot(1:10, wss, type = "b", pch = 19, 
     xlab = "Number of Clusters (K)", 
     ylab = "Total Within-Cluster Sum of Squares", 
     main = "Sum of square error (SSE) plot")

# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering with 4 clusters 
kmeans_result <- kmeans(vehicles_pca, centers = 4, nstart = 25)

# Combine cluster results with first two PCs
cluster_data <- data.frame(
  PC1 = vehicles_pca[, 1],
  PC2 = vehicles_pca[, 2],
  cluster = as.factor(kmeans_result$cluster)
)

# Extract only the first two PCs of the cluster centers
centers_df <- data.frame(
  PC1 = kmeans_result$centers[, 1],
  PC2 = kmeans_result$centers[, 2]
)

# Plot
ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_point(data = centers_df, aes(x = PC1, y = PC2),
             color = "black", size = 6, shape = 8) +
  labs(title = "K-Means Clustering (k = 4)",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


# Calculate the silhouette scores
silhouette_score_4 <- silhouette(kmeans_result$cluster, dist(vehicles_pca))

# View the silhouette score
summary(silhouette_score_4)

# Plot the silhouette scores
fviz_silhouette(silhouette_score_4)


#From above sillhoutte score it is identified that the average score is overall low
#so trying to form 3 cluster instead of 4

# Set seed for reproducibility
set.seed(123)  

# Perform K-means clustering with 3 clusters 
kmeans_result <- kmeans(vehicles_pca, centers = 3, nstart = 25)

# Combine cluster results with first two PCs
cluster_data <- data.frame(
  PC1 = vehicles_pca[, 1],
  PC2 = vehicles_pca[, 2],
  cluster = as.factor(kmeans_result$cluster)
)

# Extract only the first two PCs of the cluster centers
centers_df <- data.frame(
  PC1 = kmeans_result$centers[, 1],
  PC2 = kmeans_result$centers[, 2]
)

# Plot
ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_point(data = centers_df, aes(x = PC1, y = PC2),
             color = "black", size = 6, shape = 8) +
  labs(title = "K-Means Clustering (k = 3)",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


# Calculate the silhouette scores for K =3
silhouette_score_3 <- silhouette(kmeans_result$cluster, dist(vehicles_pca))

# View the silhouette score
summary(silhouette_score_3)

# Plot the silhouette scores
fviz_silhouette(silhouette_score_3)

#=====================================================================================================
# Get # Confusion matrix to compare actual class vs k-Mean clustering, k =3 and PCA plot with clusters with classes

# Add original class to PCA data for comparison
Vehicles_pca_with_class <- vehicles_pca  
Vehicles_pca_with_class$class <- vehicles_imputed$class  

# Add k-means cluster information to the data
Vehicles_pca_with_class$cluster <- as.factor(kmeans_result$cluster)

# Confusion matrix to compare actual class vs k-Mean clustering, k =3
table(Vehicles_pca_with_class$class, Vehicles_pca_with_class$cluster)

# PCA plot with clusters and classes
ggplot(Vehicles_pca_with_class, aes(x = PC1, y = PC2, color = cluster, shape = class)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "PCA Plot: Cluster vs Original Class",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


#=====================================================================================================

#Bar chart ofthe Projection each original feature on the principle component axis

# 1. Extract the loadings (projection of features on PC axes)
loadings <- pca_result$rotation

# 2. Compute the squared loadings (cos²) – contribution of each feature to each PC
squared_loadings <- loadings^2

# 3. Compute the proportion of contribution for each feature to each PC
# This normalizes the squared loadings column-wise
contributions <- sweep(squared_loadings, 2, colSums(squared_loadings), FUN = "/")

# 4. Visualize the contribution of features to the first few PCs

# Convert to long format for ggplot
contrib_long <- melt(contributions[, 1:3])  # select top 3 PCs
colnames(contrib_long) <- c("Feature", "PC", "Contribution")

# Plot
ggplot(contrib_long, aes(x = Feature, y = Contribution, fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Feature Contributions to Principal Components",
       y = "Contribution", x = "Original Feature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#============================================================================================

# Plot for Project each original feature on the principle component axis

# Extract first two principal components
pca_2d <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_2d) <- c("PC1", "PC2")

# Add K-means cluster assignments
pca_2d$cluster <- as.factor(kmeans_result$cluster)


# Scale the loadings for better visual effect

loadings <- pca_result$rotation[, 1:2]
scaling_factor <- max(abs(pca_2d[, 1:2]))
loadings_scaled <- loadings * scaling_factor
loadings_df <- as.data.frame(loadings_scaled)
loadings_df$feature <- rownames(loadings_df)

# Plot projected feature vectors

ggplot(pca_2d, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue", alpha = 0.6, inherit.aes = FALSE) +
  geom_text_repel(data = loadings_df,
                  aes(x = PC1, y = PC2, label = feature),
                  size = 3, color = "blue", alpha = 0.8,
                  inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "PCA Clustering with Feature Projections")


#=====================================================================================

# # Plot for Project each original feature on the principle component axis with class

# Extract first two principal components
pca_2d <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_2d) <- c("PC1", "PC2")

# Add class and K-means cluster assignments
pca_2d$class <- vehicles_imputed$class  # Add actual class labels
pca_2d$cluster <- as.factor(kmeans_result$cluster)  # Add K-means cluster assignments

# Prepare loadings by  Scale for better visual effect for feature projection
loadings <- pca_result$rotation[, 1:2]
scaling_factor <- max(abs(pca_2d[, 1:2]))
loadings_scaled <- loadings * scaling_factor
loadings_df <- as.data.frame(loadings_scaled)
colnames(loadings_df) <- c("PC1", "PC2")
loadings_df$feature <- rownames(loadings_df)

# Plot projected feature vector including both clusters and class shapes

ggplot(pca_2d, aes(x = PC1, y = PC2, color = cluster, shape = class)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue", alpha = 0.6, inherit.aes = FALSE) +
  geom_text_repel(data = loadings_df,
                  aes(x = PC1, y = PC2, label = feature),
                  color = "blue", size = 3, alpha = 0.8, inherit.aes = FALSE) +
  labs(title = "PCA Clustering with Feature Projections and Original Classes",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()


