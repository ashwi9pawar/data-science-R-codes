#Hierarchical

air1 <- EastWestAirlines[-1,]
View(air1)
sum(is.na(air1))
b <- as.numeric(air1$V2)
c <- as.numeric(air1$V3)
d <- as.numeric(air1$V4)
e <- as.numeric(air1$V5)
f <- as.numeric(air1$V6)
g <- as.numeric(air1$V7)
h <- as.numeric(air1$V8)
i <- as.numeric(air1$V9)
j <- as.numeric(air1$V10)
k <- as.numeric(air1$V11)
l <- as.numeric(air1$V12)
air2 <- data.frame('v1'=b,'v2'=c,'v3'=d,'v4'=e,'v5'=f,'v6'=g,'v7'=h,'v8'=i,'v9'=j,'v10'=k,'v11'=l)
View(air2)
air3 <- scale(air2[,1:11])
View(air3)

d <- dist(air3, method = "euclidean")
fit <- hclust(d,method = "average")
plot(fit)
clusters <- cutree(fit,k=4)
final <- data.frame('X'=EastWestAirlines[-1,1],'Clusters'=clusters)
View(final)

#k means

library(factoextra)

#elbow method
fviz_nbclust(air2,kmeans,method = "wss") + labs(subtitle = "Elbow method")
#cluster algorithm building
km <- kmeans(air2,4)
km$centers
km$cluster
clust <- data.frame("Y"=EastWestAirlines[-1,1], "cluster"=km$cluster)
View(clust)

#DBSCAN

library(fpc)

set.seed(220)
Db <- dbscan(air2,eps=5000,MinPts=20)
Db
Db$cluster
table(Db$cluster, air1$V1)
plot(Db, air2, main="DBScan")
