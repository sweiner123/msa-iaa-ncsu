
#For this homework assignment, you will use the AECO_hourly data.csv energy data set 
#provided on the GitHub repository.  This data is from PMJ (https://www.pjm.com/), 
#which is a "regional transmission organization (RTO) that coordinates the movement of 
#wholesale electricity in all or parts of 13 states the District of Columbia."   
#(Source: https://www.pjm.com/).  The data is provided on an hourly basis from August 1,
#2021 to July 30, 2022 and the units are in MW (Megawatts).  For this assignment, 
#you will need to roll up the data to TOTAL daily energy usage and answer the following
#questions.

library(ggplot2)
library(ggfortify)
library(zoo)
library(TSstudio)
library(lubridate)

## load the data

ts=read.csv('https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AECO_hourly%20data.csv')
ts$date<-as.Date(ts$datetime_beginning_ept, "%m/%d/%Y")
View(ts)
str(ts)

##aggregate and set names

ts1<-setNames(aggregate(ts$mw,list(ts$date),FUN=sum),c("date","mw"))
ts1

View(ts1)
nrow(ts1)

##separate the training, validation and the test data
ts1_train<-ts1[0:343,]
ts1_valid<-ts1[344:357,]
ts1_test<-ts1[358:364,]

nrow(ts1_train)
nrow(ts1_valid)
nrow(ts1_test)

## start working on the training dataset
## start with decomposition of the dataset assuming a weekly seasonality

ts1_train$date[1]

#dec1<-ts(data=ts1_train$mw,start=c(2021,08),frequency=7)

dec <- ts(data=ts1_train$mw, start=ts1_train$date[1], frequency=7)
decompose<-stl(dec,s.window = 7)

plot(decompose)
autoplot(decompose)


dec10<-ts(data=ts1_train$mw,frequency=7)




## Just the plot

autoplot(dec)+geom_line(aes(y=decompose$time.series[,2]),color="Red")+
  ggtitle("Energy(MW) consumption per day in RFC")+
  labs(y= "Energy(MW)", x = "Time(Day)")

#overlay the trend component
autoplot(dec)+geom_line(aes(y=decompose$time.series[,2]),color="Red")+
  ggtitle("Energy(MW) consumption per day in RFC")+
  labs(y= "Energy(MW)", x = "Time(Day)")



## Range of mw for Train

Range=max(ts1_train$mw)-min(ts1_train$mw)
Range

# standard deviation of mw
sd=sd(ts1_train$mw)
sd

##creation of ESM Model

#1 Simple Exponential Smoothing 
library(stats)
library(forecast)

model=ses(dec,initial="simple",h=14)
plot(model)
summary(model)

error_ses=ts1_valid$mw-model$mean
MAE=mean(abs(error_ses))
MAE

MAPE=mean(abs(error_ses)/abs(ts1_valid$mw))
MAPE

# 2.1 Linear/Holt Exponential Smoothing

model.les<-holt(dec,initial="optimal",h=14)
summary(model.les)
plot(model.les)

error_les=ts1_valid$mw-model.les$mean
MAE=mean(abs(error_les))
MAE

MAPE=mean(abs(error_les)/abs(ts1_valid$mw))
MAPE

# 2.2 Damped Trend Exponential Smoothing

model.ldes<-holt(dec,initial="optimal",h=14,damped=TRUE)
summary(model.ldes)
plot(model.ldes)

error_ldes=ts1_valid$mw-model.ldes$mean
MAE=mean(abs(error_ldes))
MAE

MAPE=mean(abs(error_ldes)/abs(ts1_valid$mw))
MAPE

# 3 Seasonal Exponential Smoothing

# 3.1 Holt Winters Additive Exponential Smoothing ( Includes Trend)

model.hwesA<-hw(dec,seasonal = "additive")
summary(model.hwesA)
plot(model.hwesA)


model.hwesA.results<-forecast(model.hwesA,h=14)
model.hwesA.results

error=ts1_valid$mw-model.hwesA.results$mean
MAE=mean(abs(error))
MAE

MAPE=mean(abs(error)/abs(ts1_valid$mw))
MAPE
# 0.1868102

# 3.1 Holt Winters Multiplicative Exponential Smoothing ( Includes Trend)

model.hwesM<-hw(dec,seasonal = "multiplicative")
summary(model.hwesM)
plot(model.hwesM)

model.hwesM.results<-forecast(model.hwesM,h=14)
model.hwesM.results

error=ts1_valid$mw-model.hwesM.results$mean
error
MAE=mean(abs(error))
MAE

MAPE=mean(abs(error)/abs(ts1_valid$mw))
MAPE

# 0.1809158

## Quite High MAPE but less compared to additive Mape


## final with ets function
model.ets<-ets(dec)
summary(model.ets)


model.ets.results<-forecast(model.ets,h=14)
plot(model.ets.results)

error=ts1_valid$mw-model.ets.results$mean
error

MAE=mean(abs(error))
MAE

MAPE=mean(abs(error)/abs(ts1_valid$mw))
MAPE


## In comparison , till now only Holt Winters with Multiplicative only gives the best MAPE of 18%


## Once you finalize on the model - combine train and valid and use it to build the 
## the model and test it on the test data set

#combined trained and validation dataset
ts1_train_valid<-ts1[0:357,]

dec_t_v<-ts(data=ts1_train_valid$mw,start=ts1_train_valid$date[1],frequency=7)
decompose_t_v<-stl(dec_t_v,s.window = 7)
plot(decompose_t_v)

# Run Holt Winters Multiplicative Exponential Smoothing ( Includes Trend)



model.hwesM.t.v<-hw(dec_t_v,seasonal = "multiplicative")
summary(model.hwesM.t.v)
plot(model.hwesM.t.v)

model.hwesM.results.t.v<-forecast(model.hwesM.t.v,h=7)
model.hwesM.results.t.v
plot(model.hwesM.results.t.v)

error=ts1_test$mw-model.hwesM.results.t.v$mean
MAE=mean(abs(error))
MAE

MAPE=mean(abs(error)/abs(ts1_test$mw))
MAPE


## Validation gave 18% MAPE and Test Set Gave a 20% MAPE !!


## MAPE on train+valid for additive
model.hwesAdd.t.v<-hw(dec_t_v,seasonal = "additive")
summary(model.hwesAdd.t.v)
plot(model.hwesAdd.t.v)

model.hwesAdd.results.t.v<-forecast(model.hwesAdd.t.v,h=7)
model.hwesAdd.results.t.v
plot(model.hwesAdd.results.t.v)

error=ts1_test$mw-model.hwesAdd.results.t.v$mean
MAE=mean(abs(error))
MAE

MAPE=mean(abs(error)/abs(ts1_test$mw))
MAPE

