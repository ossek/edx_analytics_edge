euclideanDistance <- function(vec1,vec2){
  if(length(vec1) != length(vec2)) stop('the two vectors gotta be the same length')
  sum((vec1-vec2)^2)^(1/2)
}

v1 = c(2,3,4)
v2 = c(3,5,77)
rbs = rbind(v1,v2)
mat = as.matrix(rbind(v1,v2))

?seq_along

lapply(mat, function(val) print(paste("iter",val)))
lapply(rbs, function(val) print(paste("iter",val)))

cbs = cbind(V1 = v1,V2 = v2)
mat2 = as.matrix(cbs)
lapply(mat2, function(val) print(paste("iter",val)))

vv1 = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
vv2 = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)

(vv1 - vv2)^2

euclideanDistance(vv1,vv2)


#Video 6
movies = read.table("./movieLens.txt",header = FALSE, sep = "|",quote = "\"")
str(movies)
colnames(movies) = c("id","title","releasedate","videoreleasedate","imdb","unknown", "Action", "Adventure",
                   "Animation", "Childrens", "Comedy", "Crime",
"Documentary", "Drama", "Fantasy", "FilmNoir",
"Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller",
"War", "Western")

#remove variables
movies$id = NULL
movies$releasedate = NULL
movies$videoreleasedate = NULL
movies$imdb = NULL

#remove dupes
movies = unique(movies)
str(movies)

nrow(movies[movies$Comedy == 1,])
nrow(movies[movies$Western == 1,])
nrow(movies[movies$Romance == 1 & movies$Drama == 1,])

#video 7
#first we calculate the distances (only on cols 2-20, the genres)
distances = dist(movies[2:20],method="euclidean")
distances
#the ward method takes into account distances between clusters, using centroid distance and also variance(?) in clusters
clusterMovies = hclust(distances,method = "ward")
plot(clusterMovies)

#let's choose 10 clusters, based on dendrogram and also the fact that we want more than 2 from which to make recs
clusterGroups = cutree(clusterMovies,k=10)

#apply mean action movie count for each cluster in clusterGroups
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)

#figure out which cluster men in black is in
subset(movies,title == "Men in Black (1997)")
#this is row 257, so to find which cluster this went into:
clusterGroups[257]

cluster2 = subset(movies,clusterGroups == 2)
cluster2$title[1:10]

#QQ
clusterGroups2 = cutree(clusterMovies,k=2)

showGroupMeans <- function(clusterGroups,cols)
{
  genreAndMean <- function(col,grps)
  {
    c (col, tapply(movies[,c(col)],grps,mean))
  }
  lapply(cols,function(col) genreAndMean(col,clusterGroups))
}
showGroupMeans(clusterGroups2,colnames(movies[2:20]))
colnames(movies)

tapply(movies$Action,clusterGroups,mean)
tapply(movies[,c("Action")],clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
