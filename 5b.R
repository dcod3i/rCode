#Part-A
genes = read.csv("/Users/pavanck/Downloads/Ch10Ex11.csv", header = FALSE)

#Part-B
set.seed(2)
km.out = kmeans(genes, 2, nstart = 20)
km.out$cluster

#Part-C
set.seed(4)
km.out = kmeans(genes, 4, nstart = 20)
km.out$cluster

#Part-D
sd.data = scale(genes)
set.seed(4)
km.out = kmeans(genes, 4, nstart = 20)
km.out$cluster

#Part-E
hc.complete = hclust(as.dist(1-cor(genes)), method = "complete")
plot(hc.complete)
hc.single = hclust(as.dist(1-cor(genes)), method = "single")
plot(hc.single)
hc.average = hclust(as.dist(1-cor(genes)), method = "average")
plot(hc.average)

#Part-F
pr.out = prcomp(t(genes))
head(pr.out$rotation)
total.load = apply(pr.out$rotation, 1, sum)
index = order(abs(total.load), decreasing = TRUE)
index[1:10]
