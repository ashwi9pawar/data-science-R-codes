file.choose()
#customise normalization function
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#apply normalisation to entire data frame
turbine <- as.data.frame(lapply(gas_turbines,normalise))
dim(turbine)

#create training and test data
turbine_train <- turbine[1:12000,]
turbine_test <- turbine[12001:15039,]

#train the neuralnet model
library("neuralnet")

#simple ann with only a single hidden neuron
turbine_model <- neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,
                           data = turbine_train,act.fct="logistic",stepmax=1000000)

plot(turbine_model)

##Evaluate model performance---

#obtain model results
model_results <- compute(turbine_model, turbine_test[1:10])
#obtain predicted TEY values
predicted_TEY <- model_results$net.result

#examine correlation between predicted and actual values
cor(predicted_TEY, turbine_test$TEY)

##Improving model performance---
#a more complex neural network topoloy with five hidden neurons
turbine_model1 <- neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,
                           data = turbine_train,act.fct="logistic",hidden=c(5,2),stepmax=1000000)
plot(turbine_model1)

model_results1 <- compute(turbine_model1, turbine_test[1:10])
#obtain predicted TEY values
predicted_TEY1 <- model_results1$net.result

#examine correlation between predicted and actual values
cor(predicted_TEY1, turbine_test$TEY)
