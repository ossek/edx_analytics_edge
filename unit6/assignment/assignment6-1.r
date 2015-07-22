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

# put the clusters into a list of clusters
grouplist = (lapply(seq(7),function(grpIdx) subset(kos,clusterGroups == grpIdx)))

#how many obs in 3?
nrow(grouplist[[3]])

#which cluster has most obs? this will show us an output we can look at to see.
lapply(seq(7),function(grpIdx) c(grpIdx,nrow(grouplist[[grpIdx]])))

#1.5
# highest mean from the first group (?)
#This computes the mean frequency values of each of the words in cluster 1, 
#and then outputs the 6 words that occur the most frequently. 
#The colMeans function computes the column (word) means, the 
#sort function orders the words in increasing order of the mean values, 
#and the tail function outputs the last 6 words listed, which are the ones with the largest column means.
tail(sort(colMeans(grouplist[[1]])))

#1.6
lapply(seq(7),function(grpIdx) tail(sort(colMeans(grouplist[[grpIdx]]))))

#2.1
set.seed(1000)
kmc = kmeans(kos,centers = 7)
kmc[1]$cluster
# put the clusters into a list again.
grouplist2 = (lapply(seq(7),function(grpIdx) subset(kos,kmc[1]$cluster == grpIdx)))
#which cluster has most obs? display (cluster -> num obs)
lapply(seq(7),function(grpIdx) c(grpIdx,nrow(grouplist2[[grpIdx]])))

#2.2
lapply(seq(7),function(grpIdx) tail(sort(colMeans(grouplist2[[grpIdx]]))))

#2.3
#remembering that these are both giant vectors that are in the same order as our data frame
# and contain the cluster number for a given row
table(clusterGroups, kmc[1]$cluster)

#2.4 (see above)
#2.5
# I think to see the answer for this question (trying to see which kmeans clusters correspond to 
#  Which heirarchical ones, we can compare to the lapply output in 2.2
table(clusterGroups, kmc[1]$cluster)
nrow(grouplist2[[7]]) 
   # no heirarchical cluster (row numbers from table call) have > .5 of the nrow / observations here)

#2.6
nrow(grouplist2[[6]])
