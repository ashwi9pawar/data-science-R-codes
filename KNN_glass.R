file.choose()

prop.table(table(glass$Type))*100
dim(glass)
library(dplyr)
#create training and test data
random <- sample_n(glass,214)
View(random)
glass_train <- random[1:114,]
glass_test <- random[115:214,]

#create labesls for training and test data
train_labels <- glass_train[1:114,10]
test_labels <- glass_test[1:100,10]
train_labels
test_labels

#Evaluating the best k value which will provide most accurate prediction
library(e1071)
library(caret)
ctrl <- trainControl(method = "cv")
knn_model <- train(Type~., method="knn", data = random, trControl = ctrl, tuneGrid = expand.grid(k=c(1:30)))
knn_model

#training model on data
library(class)
glass_test_pred <- knn(train=glass_train[,-10], test = glass_test[,-10], cl = train_labels, k=1)

#Evaluating model performance
#create cross tabulation of predicted vs actual
table(test_labels,glass_test_pred)

