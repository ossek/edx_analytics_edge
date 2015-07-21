#1.1
kos = read.csv("./dailykos.csv")
str(kos)

distances = dist(kos,method="euclidean")
clusterkos = hclust(distances,method = "ward")
plot(clusterkos)

#1.3

#1.4
clusterGroups = cutree(clusterkos,k=7)

seq(7)

grouplist = (lapply(seq(7),function(grpIdx) subset(kos,clusterGroups == grpIdx)))

#how many obs in 3?
nrow(grouplist[[3]])

#which cluster has most obs?
lapply(seq(7),function(grpIdx) c(grpIdx,nrow(grouplist[[grpIdx]])))

#1.5
tail(sort(colMeans(grouplist[[1]])))

#1.6
lapply(seq(7),function(grpIdx) tail(sort(colMeans(grouplist[[grpIdx]]))))

#2.1
set.seed(1000)
kmc = kmeans(kos,centers = 7)
kmc[1]$cluster
grouplist2 = (lapply(seq(7),function(grpIdx) subset(kos,kmc[1]$cluster == grpIdx)))
#which cluster has most obs?
lapply(seq(7),function(grpIdx) c(grpIdx,nrow(grouplist2[[grpIdx]])))

#2.2
lapply(seq(7),function(grpIdx) tail(sort(colMeans(grouplist2[[grpIdx]]))))

#2.3
#remembering that these are both giant vectors that are in the same order as our data frame
# and contain the cluster number for a given row
table(clusterGroups, kmc[1]$cluster)

#3.4 (see above)
#3.5
table(clusterGroups, kmc[1]$cluster)
nrow(grouplist2[[7]]) # no heirarchical cluster (row numbers from table call) have > .5 of the nrow / observations here)

#2.6
nrow(grouplist2[[6]])
