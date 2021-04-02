salary_train <- `SalaryData_Train(1)`
salary_test <- `SalaryData_Test(1)`

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

str(salary_train)

library(kernlab)
#building model on train dataset
salary_classifier <- ksvm(Salary~., data=salary_train, kernel="vanilladot")
#building prediction by test dataset
salary_prediction <- predict(salary_classifier, salary_test)
#checking accuracy of prediction
agreement <- salary_prediction == salary_test$Salary
prop.table(table(agreement))


#training model by non linear function

#building model on train dataset
salary_classifier_rbf <- ksvm(Salary~., data=salary_train, kernel="rbfdot")
#building prediction by test dataset
salary_prediction_rbf <- predict(salary_classifier_rbf, salary_test)
#checking accuracy of prediction
agreement <- salary_prediction_rbf == salary_test$Salary
prop.table(table(agreement))

