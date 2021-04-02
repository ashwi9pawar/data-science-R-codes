library(caret)
library(C50)
set.seed(7)

#Convert sales values into categorical values
Sales1 <- cut(`Company_Data.(2)`$Sales, breaks=c(-1,4,8,12,17), labels=c("Very_low","low","high","very_high"))
`Company_Data.(2)`["NewSales"] <- Sales1
Company <- `Company_Data.(2)`[,-1]
Company$ShelveLoc <- as.factor(Company$ShelveLoc)
Company$Urban <- as.factor(Company$Urban)
Company$US <- as.factor(Company$US)
View(Company)
str(Company)

#data partition
inTraining <- createDataPartition(Company$NewSales, p=.70, list = F)
training <- Company[inTraining,]
testing <- Company[-inTraining,]

#model building
model <- C5.0(NewSales~., data=training)
summary(model)

#predict for test data
pred <- predict.C5.0(model,testing[,-11])

#Accuracy of the algorithm
a <- table(testing$NewSales,pred)
a
sum(diag(a))/sum(a)

#Visualize decision tree
plot(model)
