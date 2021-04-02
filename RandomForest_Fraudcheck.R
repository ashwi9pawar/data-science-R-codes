library(caret)
library(randomForest)
file.choose()
Fraud <- `Fraud_check.(1)`

#create a column for risky and good according to income
df <- cut(Fraud$Taxable.Income, breaks=c(0,30000,100000), labels=c("risky","good"))
Fraud["FraudCheck"] <- df

model <- randomForest(Fraud$FraudCheck~.,data=Fraud,ntree=50)
print(model) #View forest results

#Importance of the variable ~lower gini
print(importance(model))

#prediction
pred <- predict(model,Fraud[-7])
table(pred,Fraud$FraudCheck)
table(pred,Fraud$Taxable.Income)

