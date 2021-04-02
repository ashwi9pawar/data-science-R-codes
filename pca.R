wine <- read.csv("C:/Users/Admin/Documents/wine.csv")
View(wine)
wine1 <- scale(wine[,2:13])
pca <- princomp(wine1,scores = TRUE)
summary(pca)
pca$scores

plot(pca$scores[,1:2],col="Blue",cex=0.5)
text(pca$scores[,1:2], labels=c(1:178),cex=0.7)
