### -------------------- ###
### Time Series II HW II ###
### Fall-2 | Blue-19     ###
### 10/24/2022           ###
### -------------------- ###

#############
### Setup ###
#############

# Libraries

library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(seasonalview)
library(aTSA)
library(imputeTS)
library(prophet)
library(tidyverse)

# Read CSV of Energy Consumption

setwd("C:/Users/cam3r/Desktop/NCSU IAA/AA502/R/Data/Time Series II HW 2/")

energy = read.csv("C:/Users/cam3r/Desktop/NCSU IAA/AA502/R/Data/Time Series II HW 2/hrl_load_metered.csv")

#####################
### Data Cleaning ###
#####################

# Create datetime variable from datetime_beginning_ept variable and sort

energy$datetime <- strptime(energy$datetime_beginning_ept, "%m/%d/%Y %H:%M")

energy <- energy %>% arrange(energy$datetime)

# Impute the 0 MW columns by averaging the previous and following MW Values

energy$imp <- 0

for (i in 1:nrow(energy)){
  if (energy[i, "mw"] == 0){
    energy[i,"imp"] <- 1
    energy[i,"mw"] <- mean(c(energy[i - 1, "mw"], energy[i + 1, "mw"]))
  }
}

# Adjust the time (1AM to 1:01AM) for Fall DST Change to establish sequence

DST_index <- c(2259, 10995, 19899) # These are the Fall DST observations

for (i in DST_index){
  energy$imp[i] <- 2
  energy$datetime[i] <- energy$datetime[i] + 60
}

# Re-sort the data following imputation and datetime corrections

energy <- energy %>% arrange(energy$datetime)

######################################
### Create Training and Validation ###
######################################

train <- energy[1:(28080-168),]
tail(train)

valid <- energy[(28080-167):28080,]
head(valid)

#####################
### Prophet Model ###
#####################

# Build dataframe with ds and y col names

df <- data.frame(ds = train$datetime, y = train$mw)

# Build Prophet Model

prophet_train <- prophet(daily.seasonality = TRUE)

# Fit The Model

prophet_train <- fit.prophet(prophet_train, df)

# Create Forecast

future_date_df <- make_future_dataframe(prophet_train, periods = 168, freq = 3600)

prediction <- predict(prophet_train, future_date_df)

# Calculate prediction errors from forecast

preds <- tail(predict(prophet_train, future_date_df)$yhat, 168)

prophet_error <- valid$mw - preds

# Calculate prediction error statistics (MAE and MAPE)
prophet_MAE <- mean(abs(prophet_error))
prophet_MAPE <- mean(abs(prophet_error)/abs(valid$mw))*100

prophet_MAE
prophet_MAPE

# Plot Components

prophet_plot_components(prophet_train, prediction)

# Plot Model Over Valid

plot(prophet_train, prediction)

#############################
### Plotting the forecast ###
#############################

# Build a Plotting df

plot_source <- data.frame(valid$datetime[]) # pull the date time variable for the last 168 hours into your new table

plot_source$forecast <- preds # Grab the $mean column from your forcast

plot_source$actual <- valid$mw # Grab the values from your validation TS object

names(plot_source)[1] <- "Hour" # Rename columns
names(plot_source)[2] <- "Forecast"
names(plot_source)[3] <- "Actual"

plot_source_long <- pivot_longer(plot_source, cols = Forecast:Actual, names_to = "Source", values_to = "Value") # Convert to 'long' form to help with plotting

# Graph forecast of ARIMA against Validation

ggplot(plot_source_long, aes(Hour, Value, group = Source, color = Source)) + # use 'long' table
  geom_line() + # plot lines
  scale_color_manual(values = c("black","deeppink")) + # set colors of your lines
  labs(title = "Energy Usage with Prophet Forecast Overlay (Oct. 7 - 13, 2022)", x = "Date", y = "Energy Usage (Megawatts)") # labels

###########################
### Time Series Objects ###
###########################

# Create TS Object

firstHour <- 24*(as.Date("2019-08-01 00:00:00")-as.Date("2019-1-1 00:00:00"))

ts_energy <- ts(energy$mw, start = c(2019,firstHour), frequency = 24)

# Create training set from overall energy Data
ts_train <- subset(ts_energy, end = length(ts_energy)-168)
autoplot(ts_train)

# Create valid set from overall energy Data
ts_valid <- subset(ts_energy, start = length(ts_energy)-167)
autoplot(ts_valid)

############################
### Neural Network Model ###
############################

# Check ACF/PACF

acf(ts_train)
pacf(ts_train)

# Set random seed
set.seed(12345)

# Build model

NN.Model1 <- nnetar(diff(ts_train, 24), p = 0, P = 1)  # NNAR(0,1,2)[24]
NN.Model2 <- nnetar(diff(ts_train, 24), p = 1, P = 1)  # NNAR(1,1,2)[24]
NN.Model3 <- nnetar(diff(ts_train, 24), p = 1, P = 2)  # NNAR(1,2,2)[24]
NN.Model4 <- nnetar(diff(ts_train, 24), p = 1, P = 3)  # NNAR(1,3,2)[24]
NN.Model5 <- nnetar(diff(ts_train, 24), p = 2, P = 1)  # NNAR(2,1,2)[24]
NN.Model6 <- nnetar(diff(ts_train, 24), p = 2, P = 2)  # NNAR(2,2,2)[24]
NN.Model7 <- nnetar(diff(ts_train, 24), p = 2, P = 3)  # NNAR(2,3,3)[24]
NN.Model8 <- nnetar(diff(ts_train, 24), p = 3, P = 1)  # NNAR(3,1,2)[24]
NN.Model9 <- nnetar(diff(ts_train, 24), p = 3, P = 2)  # NNAR(3,2,3)[24]
NN.Model10 <- nnetar(diff(ts_train, 24), p = 3, P = 3) # NNAR(3,3,4)[24]

# Forecast (Using best NNAR model above (8))
NN.Forecast <- forecast::forecast(NN.Model8, h = 168)
plot(NN.Forecast)

Pass.Forecast <- rep(NA, 168)

for(i in 1:168){
  Pass.Forecast[i] <- ts_train[length(ts_train) - 168 + i] + forecast::forecast(NN.Model8, h = 168)$mean[i] # (Using best NNAR model above (8))
}

Pass.Forecast <- ts(Pass.Forecast, start = c(2019,firstHour), frequency = 24)

plot(ts_train, main = "Energy Usage (Neural Network)", xlab = "Date", ylab = "Energy Usage (Megawatts)")

# Calculate prediction errors from forecast

NN.error <- ts_valid[1:168] - Pass.Forecast[1:168]

# Calculate prediction error statistics (MAE and MAPE) (Using best NNAR model above (8))
NN.MAE8 <- mean(abs(NN.error))
NN.MAPE8 <- mean(abs(NN.error)/abs(ts_valid[1:168]))*100

NN.MAE8
NN.MAPE8

##################
### Plot NNAR ###
##################

plot_source <- data.frame(energy$datetime[27913:28080]) # pull the date time variable for the last 168 hours into your new table

for_plot <- forecast::forecast(NN.Model8, h = 168) # forecast from your selected ARIMA model (Using best NNAR model above (8))

plot_source$forecast <- Pass.Forecast[1:168] # Grab the $mean column from your forecast

plot_source$actual <- ts_valid # Grab the values from your validation TS object

names(plot_source)[1] <- "Hour" # Rename columns
names(plot_source)[2] <- "Forecast"
names(plot_source)[3] <- "Actual"

plot_source_long <- pivot_longer(plot_source, cols = Forecast:Actual, names_to = "Source", values_to = "Value") # Convert to 'long' form to help with plotting

# Graph forecast of ARIMA against Validation

ggplot(plot_source_long, aes(Hour, Value, group = Source, color = Source)) + # use 'long' table
  geom_line() + # plot lines
  scale_color_manual(values = c("black","deeppink")) + # set colors of your lines
  labs(title = "Energy Usage with NNAR(3,1,2)[24] Forecast Overlay (Oct. 7 - 13, 2022)", x = "Date", y = "Energy Usage (Megawatts)") # labels

##########################
### Plot All Forecasts ###
##########################

plot_source <- data.frame(energy$datetime[27913:28080]) # pull the date time variable for the last 168 hours into your new table

for_plot1 <- tail(predict(prophet_train, future_date_df)$yhat, 168) # forecast from your Proph

for_plot2 <- Pass.Forecast[1:168] # forecast from your NNAR

plot_source$for_Proph <- for_plot1 # Add Proph forecast

plot_source$for_NNAR <- for_plot2 # Add NNAR forecast

plot_source$actual <- ts_valid # Grab the values from your validation TS object

names(plot_source)[1] <- "Hour" # Rename columns
names(plot_source)[2] <- "Prophet Forecast"
names(plot_source)[3] <- "NNAR Forecast"
names(plot_source)[4] <- "Actual"

plot_source_long <- pivot_longer(plot_source, cols = "Prophet Forecast":"Actual", names_to = "Source", values_to = "Value") # Convert to 'long' form to help with plotting

# Graph forecast of ARIMA against Validation

ggplot(plot_source_long, aes(Hour, Value, group = Source, color = Source)) + # use 'long' table
  geom_line() + # plot lines
  scale_color_manual(values = c("black","cyan3","deeppink")) + # set colors of your lines
  labs(title = "Energy Usage with NNAR and Prophet Forecast Overlay (Oct. 7 - 13, 2022)", x = "Date", y = "Energy Usage (Megawatts)") # labels

