setwd('~/Desktop/OReillyHealthcareData/code/Unsupervised/categorical_clustering')

require(klaR)
require(data.table)

data = fread('race_cardio_stress.csv')

clusters = kmodes(data, 2, iter.max = 10, weighted = FALSE)

data[, cluster:= clusters$cluster]
unique(data[order(cluster)])

data[, lapply(.SD, mean), by = cluster]

clusters$modes

