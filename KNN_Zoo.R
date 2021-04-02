file.choose()
prop.table(table(Zoo$type))*100
dim(Zoo)
Zoo <- Zoo[,-1]

#create training and test data
zoo_train <- Zoo[1:51,]
zoo_test <- Zoo[52:101,]

#create labesls for training and test data
zoo_train_labels <- Zoo[1:51,17]
zoo_test_labels <- Zoo[52:101,17]
zoo_train_labels

#Evaluating the best k value which will provide most accurate prediction
library(e1071)
library(caret)
ctrl <- trainControl(method = "cv")
knn_model <- train(type~., method="knn", data = Zoo, trControl = ctrl, tuneGrid = expand.grid(k=c(1:30)))
knn_model

#training model on data
library(class)
zoo_test_pred <- knn(train=zoo_train[,-17], test = zoo_test[,-17], cl = zoo_train_labels, k=1)

#Evaluating model performance
#create cross tabulation of predicted vs actual
table(zoo_test_labels,zoo_test_pred)

