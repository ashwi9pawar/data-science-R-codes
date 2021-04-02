library(caret)
library(C50)
set.seed(7)

#Converting Taxable income to categorical values
df <- cut(Fraud$Taxable.Income, breaks=c(0,30000,100000), labels=c("risky","good"))
Fraud["FraudCheck"] <- df
Fraud1 <- Fraud[,-3]
Fraud1$Undergrad <- as.factor(Fraud1$Undergrad)
Fraud1$Marital.Status <- as.factor(Fraud1$Marital.Status)
Fraud1$Urban <- as.factor(Fraud1$Urban)
str(Fraud1)

#data partition
inTraining <- createDataPartition(Fraud1$FraudCheck, p=.70, list = F)
training <- Fraud1[inTraining,]
testing <- Fraud1[-inTraining,]

#model building
model <- C5.0(training[,-6], training$FraudCheck)
summary(model)

#predict for test data
pred <- predict.C5.0(model,testing[,-6])

#Accuracy of the algorithm
a <- table(testing$FraudCheck,pred)
a
sum(diag(a))/sum(a)

#Visualise decision tree
plot(model)

