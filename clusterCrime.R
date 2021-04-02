#Hierarchical

crime1 <- crime_data
crime <- scale(crime1[,2:5])
View(crime)
d <- dist(crime, method = "euclidean")
fit <- hclust(d,method = "average")
plot(fit)
clusters <- cutree(fit,k=5)
final <- data.frame('X'=crime1[,1],'Clusters'=clusters)
View(final)

#k means

library(factoextra)

#elbow method
fviz_nbclust(crime1[,-1],kmeans,method = "wss") + labs(subtitle = "Elbow method")

#cluster algorithm building
km <- kmeans(crime1[,-1],3)
km$centers
km$cluster
clust <- data.frame("Y"=crime1[,1], "cluster"=km$cluster)
View(clust)

#DBSCAN

library(fpc)
crime2 <- crime1[,-1]
set.seed(220)
Db <- dbscan(crime2,eps=20,MinPts=4)
Db
Db$cluster
table(Db$cluster, crime1$X)
plot(Db, crime2, main="DBScan")

