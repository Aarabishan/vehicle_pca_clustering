# Install the necessary packages
install.packages(c("tidyverse", "cluster", "factoextra", "GGally"))

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(GGally)
library(dplyr)
library(dplyr)
library(ggplot2)

# loading the csv file
vehicles_dataset <- read.csv("C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Question_2/vehicles.csv")

# View structure
str(vehicles_dataset)
summary(vehicles_dataset)

# Check for missing values
colSums(is.na(vehicles_dataset))

# Check for "?" in columns
sapply(vehicles_dataset, function(x) if(is.character(x)) any(x == "?", na.rm = TRUE) else FALSE)

# Check for "??" in columns
sapply(vehicles_dataset, function(x) if(is.character(x)) any(x == "??", na.rm = TRUE) else FALSE)

# Count "?" values
sum(sapply(vehicles_dataset, function(x) if(is.character(x)) sum(x == "?", na.rm = TRUE) else 0))

# Count "??" values
sum(sapply(vehicles_dataset, function(x) if(is.character(x)) sum(x == "??", na.rm = TRUE) else 0))


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

# Remove the 'class' column temporarily for standardization
vehicle_features <- vehicles_imputed[, -which(names(vehicles_imputed) == "class")]

# Standardize using scale()
vehicle_scaled <- scale(vehicle_features)

# You can check the result
head(vehicle_scaled)

#=======================Dimensionality reduction using PCA=====================

# Perform PCA
pca_result <- prcomp(vehicle_scaled, center = TRUE, scale. = TRUE)

# View explained variance
summary(pca_result)


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

#write.csv(vehicles_pca, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Question_2/vehicles_pca2.csv" ) 



#=======================================Agglomerative Hierarchical Clustering===============================

#comparative dendrogram plots

# Arrange plots in 2x2 grid
par(mfrow = c(2, 2))  

# Compute distance matrix
dist_matrix <- dist(vehicles_pca)

# Single linkage
hc_single <- hclust(dist_matrix, method = "single")
plot(hc_single, main = "Single Linkage", xlab = "", sub = "", labels = FALSE, hang = -1)

# Complete linkage
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", labels = FALSE, hang = -1)

# Average linkage
hc_average <- hclust(dist_matrix, method = "average")
plot(hc_average, main = "Average Linkage", xlab = "", sub = "", labels = FALSE, hang = -1)

# Ward's method
hc_ward <- hclust(dist_matrix, method = "ward.D2")
plot(hc_ward, main = "Ward's Method", xlab = "", sub = "", labels = FALSE, hang = -1)

#===========================================================================================

# Agglomerative Hierarchical Clustering

# Compute distance matrix
dist_matrix <- dist(vehicles_pca)

# Hierarchical clustering (Ward’s method)
hc <- hclust(dist_matrix, method = "ward.D2")



# Create Dendrogram for 4 clusters

#Cut the dendrogram into 4 clusters
clusters_4 <- cutree(hc, k = 4) 
table(clusters_4)

# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram - Hierarchical Clustering")

# Add colored rectangles
rect.hclust(hc, k = 4, border = 2:5)  

#Compute sillheoutte score for 4 clusters

sil_score_4 <- silhouette(clusters_4, dist(vehicles_pca[, 1:5]))

# Summary of silhouette score for K=4 
summary(sil_score_4)

# Plot the silhouette scores for K=4 
fviz_silhouette(sil_score_4)



# Create Dendrogram for 3clusters

# Compute distance matrix
dist_matrix <- dist(vehicles_pca)

# Cut the dendrogram into 3 clusters
clusters_3 <- cutree(hc, k = 3) 
table(clusters_3)

# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

# Add colored rectangles
rect.hclust(hc, k = 3, border = 2:5)  

#Compute sillheoutte score for 3 clusters
sil_score_3 <- silhouette(clusters_3, dist(vehicles_pca[, 1:5]))

# Summary of silhouette score for K=3
summary(sil_score_3)

# Plot the silhouette scores for K=3
fviz_silhouette(sil_score_3)

#======================================================================

#Evaluation using confusion matrix for 3 clusters

# Combine PCA data, cluster assignments, and true labels

#Create dataframe
pca_data <- as.data.frame(vehicles_pca)
# Add cluster labels from hierarchical clustering
pca_data$cluster <- as.factor(clusters_3)  
#Add class label to dataframe
pca_data$class <- vehicles_imputed$class

# Confusion matrix for K=3 to compare actual class vs hierarchical clustering
table(pca_data$class, pca_data$cluster)

#Visualize the clustering on PCA-reduced data
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, shape = class)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "PCA Plot: Hierarchical Clustering vs Original Classes",
       x = "PC1", y = "PC2") +
  theme_minimal()



#Evaluation using confusion matrix for 4 clusters

# Combine PCA data, cluster assignments, and true labels
#Create dataframe
pca_data_4 <- as.data.frame(vehicles_pca)
# Add cluster labels from hierarchical clustering
pca_data_4$cluster <- as.factor(clusters_4)  
#Add class label to dataframe
pca_data_4$class <- vehicles_imputed$class

# Confusion matrix for K=4 to compare actual class vs hierarchical clustering
table(pca_data_4$class, pca_data_4$cluster)

#Visualize the clustering on PCA-reduced data
ggplot(pca_data_4, aes(x = PC1, y = PC2, color = cluster, shape = class)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "PCA Plot: Hierarchical Clustering vs Original Classes",
       x = "PC1", y = "PC2") +
  theme_minimal()

#===========================================================================================

# 1. Get the PCA loadings
loadings <- pca_result$rotation  # Each column corresponds to a principal component

# View the loadings for the first few PCs
print(round(loadings[, 1:3], 3))

# 2. Create a biplot: projects both individuals and variables onto the first two PCs
biplot(pca_result, scale = 0, main = "PCA Biplot: Feature Projections on PC1 and PC2")

# 3. Visualize feature contributions to each PC 

# Plot the variable loadings (correlation circle)
fviz_pca_var(pca_result,
             col.var = "contrib", # Color by contribution to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,        # Avoid text overlap
             title = "Feature Contribution to Principal Components")




# 1. Extract the loadings (projection of features on PC axes)
loadings <- pca_result$rotation

# 2. Compute the squared loadings (cos²) – contribution of each feature to each PC
squared_loadings <- loadings^2

# 3. Compute the proportion of contribution for each feature to each PC
# This normalizes the squared loadings column-wise
contributions <- sweep(squared_loadings, 2, colSums(squared_loadings), FUN = "/")

# 4. Visualize the contribution of features to the first few PCs
library(ggplot2)
library(reshape2)

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




