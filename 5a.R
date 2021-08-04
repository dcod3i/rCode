#a
#Perform K-means clustering of the observations with K = 3.
set.seed(3)
km.out=kmeans (USArrests,3, nstart =20)
km.out$cluster
table(km.out$cluster)

#b
# Perform K-means clustering with K = 2
set.seed(2)
km.out=kmeans(USArrests,2, nstart =20)
km.out$cluster


#Perform K-means clustering with K = 4,
set.seed(4)
km.out=kmeans (USArrests,4, nstart =20)
km.out$cluster

#c
# Use scale() function, perform K-means clustering with K = 3 USArrests
sd.data=scale(USArrests )
set.seed(3)
km.out=kmeans (USArrests,3, nstart =20)
km.out$cluster

#d
# USArrests data perform hierarchical clustering on the states.
states=row.names(USArrests )
states
table(states)

# Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist),labels=states, main="Complete Linkage", xlab="", sub="",ylab="")

#e
# Cut the dendrogram at a height that results in three distinct clusters.
hc.out=hclust(dist(sd.data))
hc.clusters =cutree (hc.out ,3)
table(states)
table(hc.clusters,states)
par(mfrow=c(1,1))
plot(hc.out , labels=states)
abline(h=3, col="red")
hc.out

#f
# Hierarchically cluster the states using complete linkage and Euclidean distance, sd.data=scale(USArrests )
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=states , main="Complete Linkage", xlab="", sub="",ylab="")
