install.packages("Metrics")
library(Metrics)
airlines <- `Airlines+Data`

plot(airlines$Passengers, type='l')
#Creating 12 dummy variables

X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==")+0)
colnames(X) <- month.abb
View(X)
air <- cbind(airlines,X)
View(air)
air["t"] <- 1:96
View(air)
air["log_passengers"] <- log(air["Passengers"])
View(air)
air["t_square"] <- air["t"]*air["t"]
View(air)

#data partition
train <- air[1:70,]
test <- air[71:96,]

#-----------Linear model----------

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_liear #48.30986

#----------Exponential model-------

expo_model<-lm(log_passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo  #43.47847

#--------Quadratic model--------

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #43.89814

#----------Additive Seasonality--------

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #124.9757

#---------Additive Seasonality with Quadratic---------

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #30.39304

#-----------Multiplicative Seasonality-------------

multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #129.6291

#-------------Multiplicative Seasonality Linear trend-------------

multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #11.72479

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#multiplicative seasonality linear trend has the least rmse

new_model <- lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = air)

resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  

# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)

#-----------predicting new data------------

library(readxl)
test_data<-read_excel(file.choose(),1)
test_data["t_square"] <- test_data["t"]*test_data["t"]
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)
pred_new["predicted passengers"] <- exp(pred_new$fit)
View(pred_new)
