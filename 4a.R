#PRINCIPAL COMPONENTS ANALYSIS
library(ISLR)
summary(USArrests)
row.names(USArrests)
names(USArrests)

# mean and variance
apply(USArrests, 2, mean)
apply(USArrests,2,var)

#find Principal Components
pr.out=prcomp(USArrests,scale. = TRUE)
names(pr.out)

#print components
pr.out$center
dim(pr.out$x)

#plot the components
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out,scale = 0)

# Proportion of Variance Explained (PVE)
#a) Using the sdev output of the prcomp()
pr.out$sdev
pr.variance=pr.out$sdev^2
pr.variance
pve=pr.variance/sum(pr.variance)
pve

#b) use loadings
pca=prcomp(USArrests,scale=TRUE)
loadings=pca$rotation
USArrests2=scale(USArrests)
sumvar=sum(apply(as.matrix(USArrests2)^2, 2, sum))
pve2=apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum) / sumvar
cumsum(pve2)

# plot the Proportion ofVariance Explained (PVE)
plot(pve , xlab=" Principal Component ", ylab="Proportion ofVariance Explained ", ylim=c(0,1),type="b")
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ",ylim=c(0,1),type="b")

