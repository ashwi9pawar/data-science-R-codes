forestfires <- `forestfires.(1)`
dim(forestfires)

#converting characters to factors
forestfires$month <- as.factor(forestfires$month)
forestfires$day <- as.factor(forestfires$day)
forestfires$size_category <- as.factor(forestfires$size_category)

#dividing dataframe for training and testing model
forest_train <- forestfires[1:400,]
forest_test <- forestfires[401:517,]

forest_train1 <- forest_train[,-28]  #removing constant column
forest_test1 <- forest_test[,-28]

library(kernlab)
#building model on train dataset
forest_classifier <- ksvm(size_category~., data=forest_train1, kernel="vanilladot")
#building prediction by test dataset
forest_prediction <- predict(forest_classifier, forest_test1)
#checking accuracy of prediction
agreement <- forest_prediction == forest_test1$size_category
prop.table(table(agreement))


#training model by non linear function
forest_classifier_rbf <- ksvm(size_category~., data=forest_train1, kernel="rbfdot")
forest_prediction_rbf <- predict(forest_classifier_rbf, forest_test1)

agreement <- forest_prediction_rbf == forest_test1$size_category
prop.table(table(agreement))

