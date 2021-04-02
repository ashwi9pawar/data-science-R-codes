install.packages("forecast","fpp","smooth","tseries")
library(forecast)
library(fpp)
library(smooth)
library(tseries)

cocacola <- CocaCola_Sales_Rawdata
View(cocacola)

#converting data into time series object
cocats <- ts(cocacola$Sales, frequency = 4, start =c(86))
View(cocats)
plot(cocats)

#data partition
train <- cocats[1:38]
test <- cocats[39:42]

#converting time series object
train <- ts(train, frequency=4)
test <- ts(test, frequency = 4)

#----------Using HOLT WINTERS MODEL------------

hwa <- HoltWinters(train,alpha = 0.2,beta = F, gamma=F)
hwa_pred <- data.frame(predict(hwa,n.ahead=4))
hwa_pred
plot(forecast(hwa,h=4))
hwa_mape <- MAPE(hwa_pred$fit,test)*100
hwa_mape

#with alpha=0.2, beta=0.1
hwab <- HoltWinters(train,alpha = 0.2,beta = 0.1, gamma=F)
hwab_pred <- data.frame(predict(hwab,n.ahead=4))
hwab_pred
plot(forecast(hwab,h=4))
hwab_mape <- MAPE(hwab_pred$fit,test)*100
hwab_mape

#with alpha=0.2, beta=0.1, gamma=0.1
hwabg <- HoltWinters(train,alpha = 0.2,beta = 0.1, gamma=0.1)
hwabg_pred <- data.frame(predict(hwabg,n.ahead=4))
hwabg_pred
plot(forecast(hwabg,h=4))
hwabg_mape <- MAPE(hwabg_pred$fit,test)*100
hwabg_mape

#with optimum values
hw_na <- HoltWinters(train,beta=F,gamma=F)
hwna_pred <- data.frame(predict(hw_na,n.ahead=4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape <- MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab <- HoltWinters(train,gamma=F)
hwnab_pred <- data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape <- MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg <- HoltWinters(train)
hwnabg_pred <- data.frame(predict(hw_nabg,n.ahead=4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape <- MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape

df_mape <- data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),
                      c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(df_mape) <- c("MAPE","VALUES")
View(df_mape)

#based on mape values we choose holt winters no alpha, no beta, no gamma model
new_model <- HoltWinters(cocats)
plot(forecast(new_model,h=4))
forecast_new <- data.frame(forecast(new_model,h=4))
forecast_new

#--------------HOLT'S METHOD------------

holt1 <- holt(train,h=4)
holt_pred <- data.frame(predict(holt1,n.ahead=4)) #train data
holt_pred
plot(forecast(holt1,h=4))
holt_pred["test"] <- test
holt_pred


holt1 <- holt(cocats,h=4)
holt_pred <- data.frame(predict(holt1,n.ahead=4)) #original data
holt_pred
plot(forecast(holt1,h=4))

#---------------SES METHOD-------------
SES1 <- ses(train, h=4)
ses_pred <- data.frame(predict(SES1,n.ahead=4)) #train data
ses_pred
plot(forecast(SES1,h=4))
ses_pred["test"] <- test
ses_pred

SES1 <- ses(cocats, h=4)
ses_pred1 <- data.frame(predict(SES1,n.ahead=4))#original data
ses_pred1
plot(forecast(SES1,h=4))


#--------------auto ARIMA model-----
plot(train)
acf(train)
pacf(train)
library(forecast)

#forecasting train data
model_AA <- auto.arima(train)
model_AA
acf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=4), xaxt="n")
forecast_new <- data.frame(forecast(model_AA,h=4), xaxt="n")
forecast_new["test"] <- test
forecast_new

#forecasting original data
model_AA1 <- auto.arima(cocats)
model_AA1
acf(model_AA1$residuals)
windows()
plot(forecast(model_AA1,h=4), xaxt="n")
forecast_new <- data.frame(forecast(model_AA1,h=4), xaxt="n")
forecast_new
