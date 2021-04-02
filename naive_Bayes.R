salary_train <- SalaryData_Train
salary_test <- SalaryData_Test
dim(salary_train)

#convert characters to factors for train data
salary_train$workclass <- as.factor(salary_train$workclass)
salary_train$education <- as.factor(salary_train$education)
salary_train$maritalstatus <- as.factor(salary_train$maritalstatus)
salary_train$occupation <- as.factor(salary_train$occupation)
salary_train$relationship <- as.factor(salary_train$relationship)
salary_train$race <- as.factor(salary_train$race)
salary_train$sex <- as.factor(salary_train$sex)
salary_train$native <- as.factor(salary_train$native)
salary_train$Salary <- as.factor(salary_train$Salary)

#convert characters to factors for test data
salary_test$workclass <- as.factor(salary_test$workclass)
salary_test$education <- as.factor(salary_test$education)
salary_test$maritalstatus <- as.factor(salary_test$maritalstatus)
salary_test$occupation <- as.factor(salary_test$occupation)
salary_test$relationship <- as.factor(salary_test$relationship)
salary_test$race <- as.factor(salary_test$race)
salary_test$sex <- as.factor(salary_test$sex)
salary_test$native <- as.factor(salary_test$native)
salary_test$Salary <- as.factor(salary_test$Salary)

library(e1071)
model<-naiveBayes(salary_train$Salary~.,data = salary_train[,-14])
pred<-predict(model,salary_test[,-14])
mean(pred==salary_test[,14])
table(pred)
table(salary_test[,14])
salary_test[,"prediction"] <- pred
View(salary_test[,c(14,15)])
