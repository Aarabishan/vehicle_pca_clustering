# Vehicle Clustering Analysis (R) - Datamining Coursework

🎯 This project applies unsupervised learning techniques to the **Vehicles Silhouettes dataset** to discover natural groupings of vehicles based on their shape-related features. The analysis uses:

- **Principal Component Analysis (PCA)** for dimensionality reduction  
- **K-means clustering**  
- **Agglomerative hierarchical clustering (Ward’s method)**

The goal is to reduce dimensionality while retaining most of the variance, identify meaningful clusters, and evaluate how well these clusters correspond to the actual vehicle classes (e.g., car, van, bus).

## 📌 1. Project Overview

The analysis focuses on:

- Cleaning and preprocessing the dataset (handling missing values, outliers, and scaling)
- Using **PCA** to transform the original features into a smaller set of uncorrelated components
- Applying **K-means** and **hierarchical clustering** on the PCA-transformed data
- Evaluating cluster quality using:
  - **Within-cluster sum of squares (WSS) / Elbow method**
  - **Silhouette scores**
  - **Confusion matrices** (cluster labels vs. true classes)
- Interpreting principal components and key features that drive separation between clusters
- Discussing the **business value** of clustering for vehicle-related applications

## 🗂️ 2. Repository Structure

The repository contains the following main scripts:

- `kmeans_clustering.R`  
  - Loads and preprocesses the dataset  
  - Handles missing values using class-wise median imputation  
  - Standardizes numeric features  
  - Performs PCA and selects the first principal components (covering ~90% variance)  
  - Runs K-means clustering (K = 4 and K = 3)  
  - Computes and interprets silhouette scores  
  - Compares cluster assignments with true vehicle classes using a confusion matrix  
  - Visualizes clusters in PCA space

- `hierarchical_clustering.R`  
  - Uses the same preprocessed and PCA-transformed data  
  - Applies agglomerative hierarchical clustering with **Ward’s linkage**  
  - Plots dendrograms for K = 4 and K = 3  
  - Evaluates cluster quality using silhouette analysis and confusion matrices  
  - Compares different linkage methods and justifies using Ward’s method  

> 🔴 **Note:** Only the R scripts (code) are included. Plots and report outputs are generated when you run the scripts in RStudio.

## 🧬 4. PCA & Dimensionality Reduction

- PCA is performed on the standardized features.
- The **scree plot** and **cumulative variance plot** are used to select the number of components.
- The first **5 principal components** are retained, capturing ~90–95% of the total variance.
- These PCs are then used as input to both K-means and hierarchical clustering.

## 🧩 5. Clustering & Evaluation (High-Level Results)

### K-means Clustering

- Initial choice from elbow/WSS plot: **K = 4**
- Silhouette analysis showed:
  - One weak cluster (very low silhouette score, ~0.06)
  - Overall average silhouette score was low (~0.29)
- After re-evaluation, **K = 3**:
  - Improved average silhouette score (~0.34)
  - More balanced and interpretable clusters
- Confusion matrix showed:
  - **Cars** are relatively well separated
  - **Vans** and **buses** overlap more in the feature space

### Hierarchical Clustering (Ward’s method)

- Dendrograms and silhouette scores were evaluated for **K = 4** and **K = 3**.
- For **K = 4**, one very small cluster (only a few samples) had a high silhouette score but was not very informative.
- For **K = 3**, cluster sizes were more balanced and silhouette widths more consistent.
- Ward’s linkage was chosen because it:
  - Minimizes within-cluster variance
  - Produces compact, spherical clusters
  - Works well with standardized, PCA-based numerical data

## 💼 6. Business Value & Applications

The clustering analysis on vehicle silhouettes can support:

- **Automated classification** of vehicle types based on shape features  
- **Inventory and fleet segmentation** (e.g., grouping similar vehicle types for operations)  
- **Computer vision and sensor-based systems** such as:
  - Traffic monitoring  
  - Automated tolling  
  - Autonomous driving  
- **Design and engineering insights**:
  - Key features like elongatedness, compactness, radius of gyration, etc., drive separation between classes  
  - Helps manufacturers and operators refine designs and maintenance strategies  
- **Data quality and feature improvement**:
  - Overlaps between true labels and clusters reveal where additional features or sensors may be needed  
  - Supports better feature engineering for future supervised models


🎓 Academic Context: This repository documents one of the applied projects completed for the Data Mining module in my MSc in Data Science programme (May 2025).All results, code, and design choices are provided for educational purposes; actual performance may vary across environments and dataset revisions.
