edges = read.csv("./edges.csv")
users = read.csv("./users.csv")
#1.1
str(edges)
str(users)

#1.2
#average number of edges from any user
mean(table(edges$V1))
mean(table(edges$V2))
#nope

table(edges$V1)
table(edges$V2)

#friendships (edges) / people (users)
nrow(edges) / length(unique(c(edges$V1,edges$V2)))
#nope

#try friendships in graph by users in users
nrow(edges) / nrow(users)
#nope

#each edge represents 2 friends or friendships, 1 for each Vertex or user
(2*nrow(edges)) / length(unique(c(edges$V1,edges$V2)))
#nope

#try friendships in graph by users in users
(2*nrow(edges)) / nrow(users)
#yep

#these are some users not in the users set but that are in edges
setdiff(users$id , unique(c(edges$V1,edges$V2)))

#1.2
table(users$school)
#oops, read more carefully
table(users$locale, users$school)

#1.3
table(users$school,users$gender)

#2.1
install.packages("igraph")
library(igraph)
?graph.data.frame
g = graph.data.frame(edges,FALSE,users)
str(g)

#plot without labels and smaller verts
plot(g, vertex.size=5, vertex.label=NA)

#2.3
Reduce(function(prev,cur) prev + cur, lapply(degree(g), function(deg) deg >= 10))

#2.4
#make vertext size a function of its degree
V(g)$size = degree(g)/2+2
#we specified size manually, so plot without it
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

#3.1
#update vertex colors
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

#3.2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

#3.3
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

#4
?igraph.plotting
