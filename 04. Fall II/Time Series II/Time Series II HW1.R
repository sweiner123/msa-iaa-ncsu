### -------------------- ###
### Time Series II HW I  ###
### Fall-2 | Blue-19     ###
### 10/12/2022           ###
### -------------------- ###

#############
### Setup ###
#############

# Libraries

library(tidyverse)
library(lubridate)
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
library(reticulate)

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

####################
### ESM Modeling ###
####################

# Building a Holt-Winters ESM - ts_energy - Additive Seasonality
hwesa_train <- hw(ts_train, seasonal = "additive", h = 168)
summary(hwesa_train)

autoplot(hwesa_train)+
  autolayer(fitted(hwesa_train),series="Fitted")+ylab("Energy Consumption")

# Building a Holt-Winters ESM - ts_energy - Multiplicative Seasonality
hwesm_train <- hw(ts_train, seasonal = "multiplicative", h = 168)
summary(hwesm_train)

autoplot(hwesm_train)+
  autolayer(fitted(hwesm_train),series="Fitted")+ylab("Energy Consumption")

# Build an ETS

ets_train <- ets(ts_train)
summary(ets_train)

autoplot(ets_train)

# Check forecast against validation set
# Calculate prediction errors from forecast

hwesa_for <- forecast::forecast(hwesa_train, h = 168)
hwesa_error <- ts_valid - hwesa_for$mean

hwesm_for <- forecast::forecast(hwesm_train, h = 168)
hwesm_error <- ts_valid - hwesm_for$mean

ets_for <- ets_train %>% forecast::forecast(h=168)
ets_error = ts_valid - ets_for$mean

# Calculate prediction error statistics (MAE and MAPE)

# HWESA
HWESA_MAE <- mean(abs(hwesa_error))
HWESA_MAPE <- mean(abs(hwesa_error)/abs(ts_valid))*100

# HWESM
HWESM_MAE <- mean(abs(hwesm_error))
HWESM_MAPE <- mean(abs(hwesm_error)/abs(ts_valid))*100

# ETS
ETS_MAE <- mean(abs(ets_error))
ETS_MAPE <- mean(abs(ets_error)/abs(ts_valid))*100

# HWESA
HWESA_MAE 
HWESA_MAPE
# HWESM
HWESM_MAE 
HWESM_MAPE
# ETS
ETS_MAE
ETS_MAPE

################
### Plot ESM ###
################

# ESM Plotting

plot_source_esm <- data.frame(energy$datetime[27745:27912]) # pull the date time variable for the last 168 hours into your new table

for_plot_esm <- forecast::forecast(hwesm_train, h = 168) # forecast from your selected ESM model

plot_source_esm$forecast <- for_plot_esm$mean # Grab the $mean column from your forcast

plot_source_esm$actual <- ts_valid # Grab the values from your validation TS object

names(plot_source_esm)[1] <- "Hour" # Rename columns
names(plot_source_esm)[2] <- "Forecast"
names(plot_source_esm)[3] <- "Actual"

# Graph forecast of ARIMA against Validation

plot_source_long_esm <- pivot_longer(plot_source_esm, cols = Forecast:Actual, names_to = "Source", values_to = "Value") # Convert to 'long' form to help with plotting

ggplot(plot_source_long_esm, aes(Hour, Value, group = Source, color = Source)) + # use 'long' table
  geom_line() + # plot lines
  scale_color_manual(values = c("black","deeppink")) + # set colors of your lines
  labs(title = "Energy Usage with Holt-Winters Multiplicative ESM Overlay (Sept. 30 - Oct. 6, 2022)", x = "Date", y = "Energy Usage (Megawatts)") # labels


######################
### ARIMA Modeling ###
######################

# Auto-ARIMA w/Season

SD.ARIMA <- auto.arima(ts_train, method="ML", seasonal = TRUE)
summary(SD.ARIMA)

# Calculate prediction errors from forecast

auto_error <- ts_valid - forecast::forecast(SD.ARIMA, h = 168)$mean

# Calculate prediction error statistics (MAE and MAPE)
auto_ARIMA.MAE <- mean(abs(auto_error))
auto_ARIMA.MAPE <- mean(abs(auto_error)/abs(ts_valid))*100

auto_ARIMA.MAE
auto_ARIMA.MAPE

# Ljung Box

checkresiduals(SD.ARIMA)

# Manual ARIMAs w/Season (Best Model)

# ARIMA(0,0,2)(3,1,2)[24]

ts_train %>% Arima(order=c(0,0,2), seasonal = c(3,1,2)) %>% residuals() %>% ggtsdisplay()

FINAL_ARIMA <- Arima(ts_train, order=c(0,0,2), seasonal = c(3,1,2))

summary(FINAL_ARIMA) # BIC=295077.4

# Ljung Box

checkresiduals(FINAL_ARIMA) # LB p-value < 2.2e-16

# Calculate Mean Error on validation

FINAL_ARIMA_error <- ts_valid - forecast::forecast(FINAL_ARIMA, h = 168)$mean

FINAL_ARIMA_MAE <- mean(abs(FINAL_ARIMA_error)) # 60.01
FINAL_ARIMA_MAPE <- mean(abs(FINAL_ARIMA_error)/abs(ts_valid))*100 # 6.60

##################
### Plot ARIMA ###
##################

plot_source <- data.frame(energy$datetime[27745:27912]) # pull the date time variable for the last 168 hours into your new table

for_plot <- forecast::forecast(FINAL_ARIMA, h = 168) # forecast from your selected ARIMA model

plot_source$forecast <- for_plot$mean # Grab the $mean column from your forcast

plot_source$actual <- ts_valid # Grab the values from your validation TS object

names(plot_source)[1] <- "Hour" # Rename columns
names(plot_source)[2] <- "Forecast"
names(plot_source)[3] <- "Actual"

plot_source_long <- pivot_longer(plot_source, cols = Forecast:Actual, names_to = "Source", values_to = "Value") # Convert to 'long' form to help with plotting

# Graph forecast of ARIMA against Validation

ggplot(plot_source_long, aes(Hour, Value, group = Source, color = Source)) + # use 'long' table
  geom_line() + # plot lines
  scale_color_manual(values = c("black","deeppink")) + # set colors of your lines
  labs(title = "Energy Usage with ARIMA(0,0,2)(3,1,2)[24] Forecast Overlay (Sept. 30 - Oct. 6, 2022)", x = "Date", y = "Energy Usage (Megawatts)") # labels

#------------------------------------------------------------------------------#

###----------###
### Appendix ###
###----------###

# Additional ESM Models

# Building a Single Exponential Smoothing (SES) Model 
ses_train <- ses(ts_train, initial = "simple", h = 168)

summary(ses_train)

autoplot(ses_train)+
  autolayer(fitted(ses_train),series="Fitted")+ylab("Energy Consumption")

# Building a Linear Exponential Smoothing Model 
les_train <- holt(ts_train, initial = "optimal", h = 168)

summary(les_train)

autoplot(les_train)+
  autolayer(fitted(les_train),series="Fitted")+labs(title="AECO Energy Consumption with Holt forecasts",y="Energy Consumption")

# Building a Damped Linear Exponential Smoothing Model
ldes_train <- holt(ts_train, initial = "optimal", h = 168, damped = TRUE)
summary(ldes_train)

autoplot(ldes_train)+
  autolayer(fitted(ldes_train),series="Fitted")+labs(title="AECO Energy Consumption with Linear Damped ESM Forecast")

# Check forecast against validation set
# Calculate prediction errors from forecast

ses_for <- forecast::forecast(ses_train, h = 168)
ses_error <- ts_valid - ses_for$mean

les_for <- forecast::forecast(les_train, h = 168)
les_error <- ts_valid - les_for$mean

ldes_for <- forecast::forecast(ldes_train, h = 168)
ldes_error <- ts_valid - ldes_for$mean

# SES
SES_MAE <- mean(abs(ses_error))
SES_MAPE <- mean(abs(ses_error)/abs(ts_valid))*100

# LES
LES_MAE <- mean(abs(les_error))
LES_MAPE <- mean(abs(les_error)/abs(ts_valid))*100

# LDES
LDES_MAE <- mean(abs(ldes_error))
LDES_MAPE <- mean(abs(ldes_error)/abs(ts_valid))*100

# SES
SES_MAE
SES_MAPE
# LES
LES_MAE 
LES_MAPE
# LDES
LDES_MAE 
LDES_MAPE 

#------------------------------------------------------------------------------#

# Additional Manual ARIMA models

# ARIMA(AR1,0,0)(0,1,1)[24]

ts_train %>% Arima(order=c(1,0,0), seasonal = c(0,1,1)) %>% residuals() %>% ggtsdisplay()

S1_ARIMA <- Arima(ts_train, order=c(1,0,0), seasonal = c(0,1,1))

summary(S1_ARIMA) # BIC = 264382.4

# Ljung Box

checkresiduals(S1_ARIMA) # LB - p-value < 2.2e-16

# Calculate Mean Error on validation

S1_ARIMA_error <- ts_valid - forecast::forecast(S1_ARIMA, h = 168)$mean

S1_ARIMA_MAE <- mean(abs(S1_ARIMA_error))
S1_ARIMA_MAPE <- mean(abs(S1_ARIMA_error)/abs(ts_valid))*100

# ARIMA(AR1,0,0)(0,1,2)[24]

ts_train %>% Arima(order=c(1,0,0), seasonal = c(0,1,2)) %>% residuals() %>% ggtsdisplay()

S2_ARIMA <- Arima(ts_train, order=c(1,0,0), seasonal = c(0,1,2))

summary(S2_ARIMA) # BIC = 263858.1

# Ljung Box

checkresiduals(S2_ARIMA) # LB p-value < 2.2e-16

# Calculate Mean Error on validation

S2_ARIMA_error <- ts_valid - forecast::forecast(S2_ARIMA, h = 168)$mean

S2_ARIMA_MAE <- mean(abs(S2_ARIMA_error))
S2_ARIMA_MAPE <- mean(abs(S2_ARIMA_error)/abs(ts_valid))*100

# ARIMA(AR1,0,0)(0,1,3)[24]

ts_train %>% Arima(order=c(1,0,0), seasonal = c(0,1,3)) %>% residuals() %>% ggtsdisplay()

S3_ARIMA <- Arima(ts_train, order=c(1,0,0), seasonal = c(0,1,3))

summary(S3_ARIMA) # BIC = 263867.6

# Ljung Box

checkresiduals(S3_ARIMA) # LB p-value < 2.2e-16

# Calculate Mean Error on validation

S3_ARIMA_error <- ts_valid - forecast::forecast(S3_ARIMA, h = 168)$mean

S3_ARIMA_MAE <- mean(abs(S3_ARIMA_error))
S3_ARIMA_MAPE <- mean(abs(S3_ARIMA_error)/abs(ts_valid))*100

# ARIMA(AR1,0,0)(1,1,2)[24]

ts_train %>% Arima(order=c(1,0,0), seasonal = c(1,1,2)) %>% residuals() %>% ggtsdisplay()

S4_ARIMA <- Arima(ts_train, order=c(1,0,0), seasonal = c(1,1,2))

summary(S4_ARIMA) # BIC = 263868.1

# Ljung Box

checkresiduals(S4_ARIMA) # LB p-value < 2.2e-16

# Calculate Mean Error on validation

S4_ARIMA_error <- ts_valid - forecast::forecast(S4_ARIMA, h = 168)$mean

S4_ARIMA_MAE <- mean(abs(S4_ARIMA_error))
S4_ARIMA_MAPE <- mean(abs(S4_ARIMA_error)/abs(ts_valid))*100

#------------------------------------------------------------------------------#

# Deterministic Seasonality ARIMA Modeling

#### Fourier Transforms

plots <- list()

for (i in seq(11)) { # seq 11 is half the variables (24 is actually 23 dummy variables, then round down)
  fit <- auto.arima(ts_train, xreg = fourier(ts_train, K = i), # K is number of pairs
                    seasonal = FALSE, lambda = NULL)
  plots[[i]] <- autoplot(forecast::forecast(fit,
                                            xreg = fourier(ts_train, K=i, h=168))) +
    xlab(paste("K=",i,"   BIC=",round(fit$bic,2))) +
    ylab("") + ylim(30000,80000)
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]],
  plots[[7]],plots[[8]],plots[[9]],
  plots[[10]],plots[[11]], nrow=4)

# plots[[6]]

F_ARIMA <- auto.arima(ts_train, xreg = fourier(ts_train, K = 6), seasonal = FALSE) # We are choosing the K value that was best from the above plots

summary(F_ARIMA)

# Calculate Mean Error on validation

F_ARIMA_error <- ts_valid - forecast::forecast(F_ARIMA, h = 168, xreg = fourier(ts_train, K = 6, h = 168))$mean

F_ARIMA_MAE <- mean(abs(F_ARIMA_error))
F_ARIMA_MAPE <- mean(abs(F_ARIMA_error)/abs(ts_valid))*100
