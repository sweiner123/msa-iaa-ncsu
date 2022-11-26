library(tidyverse)
library(ggplot2)
library(forecast)
library(lubridate, warn.conflicts = FALSE)
library(scales)
library(Metrics)

########## Code split between the auto ARIMA (first) and manual ARIMA (second) ##########

##Reading csv file
aeco <- read.csv('https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AECO_hourly%20data.csv')

##Separating date from datetime and formatting value as date
aeco2 <- aeco %>%
  separate(datetime_beginning_ept,c("date","time"),sep=" ")
aeco2$date <- mdy(str_squish(aeco2$date))

##Summarizing daily MW usage
aeco.daily <- aeco2 %>%
  group_by(date) %>%
  summarize(daily_mw = sum(mw))

#Splitting data into train, test, and validation data sets
test <- tail(aeco.daily, 7)
aeco.daily <- head(aeco.daily, -7)
validation <- tail(aeco.daily, 14)
train <- head(aeco.daily, -14)

##STL Decomposition
train.ts <- ts(train$daily_mw, frequency = 7)
decomp_stl <- stl(train.ts, s.window = 7)
autoplot(decomp_stl)
seas_adj = train.ts - decomp_stl$time.series[,1]

##Plot of Training Data
ggplot(aes(x=date, y = daily_mw),data = train) +
  geom_line(aes(y=train.ts, color="Actual", linetype="Actual"), size = 1) +
  geom_hline(aes(yintercept=mean(daily_mw), color="Mean", linetype="Mean")) +
  labs(title="Actual Electic Load Values", x = "Date", y = "Total Energy Usage (MW)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  scale_color_manual(name=" ", values = c("Actual"="black", "Mean"="blue")) +
  scale_linetype_manual(name=" ", values = c("Actual"="solid", "Mean"="dashed")) +
  theme_bw()

##Augmented Dickey-Fuller Test
aTSA::adf.test(train$daily_mw)

##Autocorrelation (MA) and Partial Autocorrelation (AR)
ggAcf(train$daily_mw, lag.max = 12)
ggPacf(train$daily_mw, lag.max = 12)

##Take Differences
diff.ARIMA <- Arima(train$daily_mw, order = c(0,1,0))
ggAcf(diff.ARIMA$residuals, lag.max = 12)
ggPacf(diff.ARIMA$residuals, lag.max = 12)

########## Auto-ARIMA Model ##########

##Auto-ARIMA Model
auto.model <- auto.arima(train$daily_mw, seasonal = F)
summary(auto.model)

##Autocorrelation and Partial Autocorrelation Residuals
ggAcf(auto.model$residuals, lag.max = 12)
ggPacf(auto.model$residuals, lag.max = 12)

##White Noise
index=seq(1,18)
White.LB <- rep(NA, 18)
for (i in 8:18){
  White.LB[i] <- Box.test(auto.model$residuals, lag=i, type = "Ljung-Box", fitdf = 8)$p.value
}
white.dat=data.frame(cbind(White.LB[8:18],index[8:18]))
colnames(white.dat)=c("pvalues","Lag")
ggplot(white.dat, aes(x=factor(Lag),y=pvalues)) +
  geom_col() +
  labs(title="Model", x="Lags", y="pvalues")
ggplot(data=train, aes(x=auto.model$residuals)) +
  geom_histogram() +
  labs(title="Histogram of Residuals for Model", x="Residuals", y="Frequency")

##Forecasting
train.auto.forecast <- forecast(auto.model, h = 14)

##Overlay Validation with Predicted
valid.auto.overlay <- validation
valid.auto.overlay$prediction <- train.auto.forecast$mean
ggplot(aes(x=date, y=daily_mw),data = valid.auto.overlay) +
  geom_line(aes(y=daily_mw, colour = "Actual"), size = 1) +
  geom_line(aes(y=prediction, colour = "Predicted"), size = 1) +
  labs(title="Actual vs. Predicted Validation Values", y="Daily Energy Usage (MW)", x="Date") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
  scale_color_manual(name = " ", values = c("Actual"="black","Predicted"="orange")) +
  theme_bw()

##MAPE, MAE, sMAPE, RMSE
mape(validation$daily_mw, train.auto.forecast$mean)
mae(validation$daily_mw, train.auto.forecast$mean)
smape(validation$daily_mw, train.auto.forecast$mean)
rmse(validation$daily_mw, train.auto.forecast$mean)

########## ARIMA Model ##########

##ARIMA Model
aeco.ARIMA <- Arima(train$daily_mw, order=c(3,1,3))
summary(aeco.ARIMA)

##Autocorrelation and Partial Autocorrelation Residuals
ggAcf(aeco.ARIMA$residuals, lag.max = 12)
ggPacf(aeco.ARIMA$residuals, lag.max = 12)

##White Noise
index=seq(1, 16)
White.LB <- rep(NA, 16)
for (i in 6:16){
  White.LB[i] <- Box.test(aeco.ARIMA$residuals, lag=i, type = "Ljung-Box", fitdf = 6)$p.value
}
white.dat=data.frame(cbind(White.LB[6:16],index[6:16]))
colnames(white.dat)=c("pvalues","Lag")
ggplot(white.dat, aes(x=factor(Lag),y=pvalues)) +
  geom_col() +
  labs(title="Model", x="Lags", y="pvalues")
ggplot(data=train, aes(x=auto.model$residuals)) +
  geom_histogram() +
  labs(title="Histogram of Residuals for Model", x="Residuals", y="Frequency")

##Forecasting
train.ARIMA.forecast <- forecast(aeco.ARIMA, h = 14)
autoplot(train.ARIMA.forecast)

##Overlay Validation with Predicted
valid.ARIMA.overlay <- validation
valid.ARIMA.overlay$prediction <- train.ARIMA.forecast$mean
valid.ARIMA.overlay$lower <- train.ARIMA.forecast$lower
valid.ARIMA.overlay$upper <- train.ARIMA.forecast$upper
ggplot(aes(x=date, y=daily_mw),data = valid.ARIMA.overlay) +
  geom_line(aes(y=daily_mw, colour = "Actual"), size = 1) +
  geom_line(aes(y=prediction, colour = "Predicted"), size = 1) +
  #geom_line(aes(y=lower[,"80%"], colour = "Interval"), size = 1, linetype=2) +
  #geom_line(aes(y=upper[,"80%"], colour = "Interval"), size = 1, linetype=2) +
  #geom_line(aes(y=lower[,"95%"], colour = "Interval"), size = 1, linetype=2) +
  #geom_line(aes(y=upper[,"95%"], colour = "Interval"), size = 1, linetype=2) +
  labs(title="Actual vs. Predicted Validation Values", y="Daily Energy Usage (MW)", x="Date") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
  scale_color_manual(name = " ", values = c("Actual"="black","Predicted"="orange")) +
  theme_bw()

##Validation set MAPE, MAE, sMAPE, RMSE
mape(validation$daily_mw, train.ARIMA.forecast$mean)
mae(validation$daily_mw, train.ARIMA.forecast$mean)
smape(validation$daily_mw, train.ARIMA.forecast$mean)
rmse(validation$daily_mw, train.ARIMA.forecast$mean)

########## Test Set ###########

##Combine Training and Validation sets
combined <- rbind(train, validation)

##ARIMA Models
combined.auto.ARIMA <- Arima(combined$daily_mw, order=c(4,1,4))
combined.ARIMA <- Arima(combined$daily_mw, order=c(3,1,3))

##Forecasts
combined.auto.ARIMA.forecast <- forecast(combined.auto.ARIMA, h = 7)
combined.ARIMA.forecast <- forecast(combined.ARIMA, h = 7)

##Auto ARIMA MAPE, MAE, sMAPE, RMSE
mape(test$daily_mw, combined.auto.ARIMA.forecast$mean)
mae(test$daily_mw, combined.auto.ARIMA.forecast$mean)
smape(test$daily_mw, combined.auto.ARIMA.forecast$mean)
rmse(test$daily_mw, combined.auto.ARIMA.forecast$mean)

##Manual ARIMA MAPE, MAE, sMAPE, RMSE
mape(test$daily_mw, combined.ARIMA.forecast$mean)
mae(test$daily_mw, combined.ARIMA.forecast$mean)
smape(test$daily_mw, combined.ARIMA.forecast$mean)
rmse(test$daily_mw, combined.ARIMA.forecast$mean)
