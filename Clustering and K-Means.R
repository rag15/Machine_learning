# Going to Gramener Site : https://gramener.com/cluster >> India Districts.
# Clustering is a Un Supervised Learing Algorithm that donot have any prior Labelled Data.

# ============ India Districts.
# All the metrics are in the form of Columns which are Features.
# The rows are districts, so we are going create clusters based on the values of rows.
# ** One main dis-advantage of Clustering is most of algorithms need an input from user for number of clusters.
# ** Algorithms such as Density Based Scan will automatically find the number of clusters based on data.

# ============== Irisi Data
data("iris")
View(iris)
library(dplyr)
# Here we perform Clustering on Iris by removing the species Column.
iris_new <- iris %>% select(-Species)

# Steps in ML Analysis.
# S1 - Transforming the Data - Not Required
# S2 - Missing and Outliers Treatment - Skipped for Now.
# S3 - Splitting the Data into Test and Train Data - Not Applicable (bcz. It is unsupervised)
# S4 - Build the Model.

iris_model <- kmeans(iris_new,centers = 3) # By Trail and Error we have 3 Clusters as of now.
length(iris_model$cluster) # The length of cluster should be equal to nrows of data frame
table(iris_model$cluster) # Gives frequency of each cluster.

# S5 - Test / Evaluate the Data -  Not Applicable (As there is No Test / Train Data)
# S6 - Performance Validation
# We will build the different models by giving different no.of clusters.

iris_model1 <- kmeans(iris_new,centers = 4)
iris_model2 <- kmeans(iris_new,centers = 2)
iris_model3 <- kmeans(iris_new,centers = 5)

# Based on the different values of betweenSS and Total_SS we take a move on No.of Clusters
# **iter (What does the Iteration Mean ?)
# Initially algorithm assumes random point as center of clusters.
# As it travels through the data it finds more points in a cluster and the center of clusters gradually moves to actual center.
