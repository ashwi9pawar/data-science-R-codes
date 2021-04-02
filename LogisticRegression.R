bank <- read.csv(file.choose())
sum(is.na(bank))
bank <- na.omit(bank) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(bank)

colnames(bank)
model <- glm(factor(y)~.,data=bank,family = "binomial")
summary(model)
exp(coef(model))
prob <- predict(model,bank,type="response")
summary(prob)
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

View(bank[,c(1,17:20)])

table(bank$y,bank$pred_values)

library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
