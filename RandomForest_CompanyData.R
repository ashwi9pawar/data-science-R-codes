library(caret)
library(randomForest)
file.choose()

#Convert Sales numerical values into categorical values
Sales1 <- cut(Company_Data$Sales, breaks=c(-1,4,8,12,17), labels=c("Very_low","low","high","very_high"))
Company_Data["NewSales"] <- Sales1
Company <- Company_Data[,-1]
View(Company)

model <- randomForest(Company$NewSales~.,data=Company,ntree=50)
print(model) #View forest results

#Importance of the variable ~lower gini
print(importance(model))

#prediction
pred <- predict(model,Company[-11])
table(pred,Company$NewSales)
