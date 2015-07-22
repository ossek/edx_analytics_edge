airline = read.csv("./AirlinesCluster.csv")
str(airline)

#1.1
lapply(colnames(airline),function(col) c(col,mean(airline[,col])))

#1.2

#1.3
install.packages("caret")
library(caret)
#preprocess
preproc = preProcess(airline)
# do normalization.  is there a time we wouldn't use this?
airlinesNorm = predict(preproc, airline)
summary(airlinesNorm)

lapply(colnames(airlinesNorm),function(col) c(col,max(airlinesNorm[,col])))

#2.1
distances = dist(airlinesNorm,method="euclidean")
clusterAirlinesNorm = hclust(distances,method = "ward")
plot(clusterAirlinesNorm)

#2.2
airlinesNormClusters = cutree(clusterAirlinesNorm,k=5)
# iterate over the list of cluster assignments and return true(1) only where a cluster assignment was 1.
# then sum up all the 1's for the count of datapoints that have been assigned to cluster 1.
Reduce(function(last,cur) last + cur, lapply(airlinesNormClusters,function(clusterNum) clusterNum == 1))

#2.3 - 2.7
#let's compare the centroids for each cluster
#  You may want to compute the average values of the unnormalized data so that it is easier to interpret.
# this will make (col,mean) pairings and then call rbind across all the (col,mean) sets
# to make a table-ish thing.
Reduce(rbind,lapply(colnames(airlinesNorm),function(col) c(col,tapply(airline[,c(col)], airlinesNormClusters, mean))))
#alternatively
#split the data into clusters then use lapply to apply the colMeans function to each cluster
lapply(split(airline, airlinesNormClusters), colMeans)

#3.1
set.seed(88)
kmcAirlinesNorm = kmeans(airlinesNorm,centers = 5,iter.max = 1000)
airlinesNormClusters2 = (lapply(seq(5),function(grpIdx) subset(airlinesNorm,kmcAirlinesNorm[1]$cluster == grpIdx)))
#which clusters have > 1000 obs?
lapply(seq(5),function(grpIdx) c(grpIdx,nrow(airlinesNormClusters2[[grpIdx]])))

#3.2
kmcAirlinesNorm$centers
#or unnormalized:
Reduce(rbind,lapply(colnames(airlinesNorm),function(col) c(col,tapply(airline[,c(col)], kmcAirlinesNorm[1]$cluster, mean))))

?kmeans
