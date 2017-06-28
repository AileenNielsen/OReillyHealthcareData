setwd('~/Desktop/OReillyHealthcareData/code/Unsupervised/mixed_types')

library(cluster)

cluster_data = fread('weight_cholesterol_categories.csv')
cluster_data[, V1:=NULL]

cluster_data[, race := as.factor(race)]
cluster_data[, education := as.factor(education)]


# dissimilarity matrix calculation
dist <- daisy(cluster_data, metric = "gower")

# partitioning around the medoids
pam_fit <- pam(dist, diss = TRUE, k = 2)

cluster_data[, group:= pam_fit$clustering]

cluster_data[, race := as.numeric(race)]
cluster_data[, education := as.numeric(education)]

cluster_data[, lapply(.SD, mean), group]