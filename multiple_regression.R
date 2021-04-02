install.packages("car")
library(car)
library(mvinfluence)
file.choose()
View(`50_Startups`)

cor(`50_Startups`)

summary(`50_Startups`)
abc <- `50_Startups`[-4]
cor(abc)
pairs(abc)
model <- lm(Profit~.,data = abc)
summary(model)

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables
vif(model) # Original model
influence.measures(model)
windows()
influenceIndexPlot(model) # index plots for infuence measures
influencePlot(model)
model2 <- lm(Profit~.,data = abc[-46,])
summary(model2)
model <- lm(Profit~.,data = abc[-47,])
summary(model)
model <- lm(Profit~.,data = abc[-49,])
summary(model)
model <- lm(Profit~.,data = abc[-50,])
summary(model)
model <- lm(Profit~.,data = abc[-c(46,50),])
summary(model)
model3 <- lm(Profit~.,data = abc[-c(46,47,49,50),])
summary(model3)


#finalmodel
finalmodel2 <- lm(Profit~.,data = abc[-c(46,50,15,16,49,37,39,13),])
summary(finalmodel2)
windows()
plot(finalmodel2)
hist(residuals(finalmodel2))
qqplot(finalmodel2)
influenceIndexPlot(finalmodel2)
influencePlot(finalmodel2)

#testdata
testdata=data.frame(R.D.Spend=200000,Administration=150000,Marketing.Spend=500000)
prediction=predict(finalmodel2,testdata)
prediction
