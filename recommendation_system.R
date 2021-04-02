library(plyr)
library(recommenderlab)
library(Matrix)
library(caTools)
books <- file.choose()
str(books)
hist(books$Book.Rating)
book1 <- books[,-1]
View(book1)
books_matrix <- as(book1,'realRatingMatrix')
books_matrix@data
book_recom <- Recommender(books_matrix,method="POPULAR")
recommend1 <- predict(book_recom, books_matrix, n=5)
d <- as(recommend1,"list")
df <- ldply(d,data.frame)
recommend1
d
df
