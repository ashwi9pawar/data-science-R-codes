file.choose()
#customise normalization function
normalise <- function(x)
  {
  return((as.numeric(x)-min(as.numeric(x)))/(max(as.numeric(x))-min(as.numeric(x))))
}
forestfires$month <- as.factor(forestfires$month)
forestfires$day <- as.factor(forestfires$day)
forestfires$size_category <- as.factor(forestfires$size_category)

#apply normalisation to entire data frame
forest <- as.data.frame(lapply(forestfires,normalise))
dim(forest)

#create training and test data
forest_train <- forest[1:400,]
forest_test <- forest[401:517,]

#train the neuralnet model
library("neuralnet")

#simple ann with only a single hidden neuron
formula_nn <- paste("size_category",paste(colnames(forestfires[-31]),collapse ="+"),sep="~")
forest_model <- neuralnet(formula = formula_nn,
                           data = forest_train,act.fct="logistic",stepmax=100000)

plot(forest_model)

##Evaluate model performance---

#obtain model results
model_results <- compute(forest_model, forest_test[1:30])
#obtain predicted TEY values
predicted_size <- model_results$net.result

#examine correlation between predicted and actual values
cor(predicted_size, forest_test$size_category)

##Improving model performance---
#a more complex neural network topoloy with five hidden neurons
forest_model1 <- neuralnet(formula = formula_nn,
                            data = forest_train,act.fct="logistic",hidden=c(5,2),stepmax=100000)
plot(forest_model1)

model_results1 <- compute(forest_model1, forest_test[1:30])
#obtain predicted TEY values
predicted_size1 <- model_results1$net.result

#examine correlation between predicted and actual values
cor(predicted_size1, forest_test$size_category)
