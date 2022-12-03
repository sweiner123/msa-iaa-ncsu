###################################
#                                 #
#         Survival Analysis:      #
#            Homework 3           #
#                                 #
#          Nikhil Suthar          #
#                                 #
###################################


# Import libraries
library(tidyverse)
library(survival)
#install.packages("survminer")
library(survminer)
#install.packages("flexsurv")
library(flexsurv)
library(car)
#install.packages("visreg")
library("visreg")

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# Failed pump percentage
table(hurricane$reason)*100/nrow(hurricane)

# 0 – no failure
# 1 – flood failure
# 2 – motor failure
# 3 – surge failure
# 4 – jammed failure

# Creating a failure variable for motor vs no motor
hurricane$motor_failure <- ifelse(hurricane$reason == 2, 1, 0)
hurricane$id <- seq(1: nrow(hurricane))

table(hurricane$motor_failure)

table(hurricane$trashrack,hurricane$motor_failure)
hurricane[432,'trashrack']<-1
table(hurricane$trashrack,hurricane$motor)

missing_hours <- sapply(hurricane[,9:56], function(x) sum(is.na(x)))
sum(missing_hours)

# Fix the data issue in columns h1-h48

# Assumption - for all the observations, we assume that the survival hour is correct. 

# Observation 442, 632 and 647 have error in data entry

#The survival hour is longer than the length of indicators within H1-H48, change the survival hour.
#hurricane[442,]$hour<-1
#hurricane[632,]$hour<-8
#hurricane[647,]$hour<-2

#remove the 3 pumps from the data
hurricane <- hurricane[-c(442, 632, 647), ]
hurricane$id <- seq(1: nrow(hurricane))

#Thus, all variables that have extra indicators within H1-H48 after the pump failed, impute them with NAs.
for (i in 1:nrow(hurricane)){
  if (hurricane[i,'hour']<length(hurricane[i,9:56][!is.na(hurricane[i,9:56])])){
    print(i) 
    
    hurricane[i,(9+hurricane[i,'hour']):56]<-NA
  }
}

# Perform Variable selection

#calculate alpha level
p<-1-pchisq(log(767),1)
alpha.f <- round(p,3)

#variable selection
full.model<-coxph(Surv(hour, motor_failure)~factor(backup)+age+factor(bridgecrane)+factor(servo)+factor(gear)+factor(trashrack)+slope+elevation, data=hurricane)
#alpha.f=0.03
back.model <- step(full.model, direction = "backward", 
                   k=qchisq(alpha.f, 1, lower.tail=FALSE))
summary(back.model)

# Select the important variables
m <- hurricane  %>% 
  select(id,age,trashrack, slope)


sum(hurricane['hour'])


# store pump hours in a vector
pump_hours <- as.numeric(hurricane$hour)

# duplicate indexes based on the list
idx <- rep(1:nrow(hurricane), pump_hours)

#duplicate rows based on the indexes
hurricane_long <- m[idx,]


start_vector <- c()
stop_vector <- c()
pump_status_vector <- c()
failure_event_vector <- c()

for (i in 1:nrow(hurricane)){
  hour <- hurricane[i,'hour']
  
  start_seq <- seq(0,hour-1,by=1)
  stop_seq <- seq(1,hour,by=1)
  
  start_vector <- c(start_vector,start_seq)
  stop_vector <- c(stop_vector,stop_seq)
  
  record <- as.numeric(hurricane[i,9:56][!is.na(hurricane[i,9:56])])
  pump_status_vector <- c(pump_status_vector, record)
  
  if(hurricane[i,'motor_failure'] == 1){
    pump_values <- c(rep(0,hour-1),1)
  }
  else{
    pump_values <- rep(0,hour)
  }
  
  failure_event_vector <- c(failure_event_vector,pump_values)
}

hurricane_long$start <- start_vector
hurricane_long$end <- stop_vector

hurricane_long$pump_status <- pump_status_vector

#add all events for each pump
hurricane_long$failure_event <- failure_event_vector

#reset row index
rownames(hurricane_long) <- 1:nrow(hurricane_long)

## Create the time dependent variable

hurricane_long$longer_than_12<-NA
for (i in 1:nrow(hurricane_long)){
  if ((hurricane_long[i,'start']>=12)){
    hurricane_long[i,"longer_than_12"]<-ifelse(sum(hurricane_long[(i-12):(i-1),'pump_status'])==12,1,0)
  }
  else {
    hurricane_long[i,"longer_than_12"]<-0
  }
}

hurricane_long<-hurricane_long %>% mutate(longer_than_12=ifelse((pump_status==0 & longer_than_12==1),0,longer_than_12))

data <- hurricane_long %>% select(id,age,slope,trashrack,start, end, failure_event,longer_than_12)

motor.ph<-coxph(Surv(start,end,failure_event)~age+factor(longer_than_12)+
                  factor(trashrack)+slope,
                data=data)
summary(motor.ph)

# Effect of the slope variable (in %)
100 * (exp(motor.ph$coefficients["age"])-1)

# Effect of the trashrack variable (in %)
100 * (exp(motor.ph$coefficients["factor(trashrack)1"])-1)

# Effect of the slope variable (in %)
100 * (exp(motor.ph$coefficients["slope"])-1)

## Martingale residuals...Linearity
survminer::ggcoxfunctional(motor.ph,data=data)
summary(motor.ph)
#Errors

#Using the visreg package

#create a function to plot residuals vs X
viz_lin<-function(model, x){
  visreg(model, x, xlab = x, ylab = "partial residuals",
         gg = TRUE, band = FALSE) +  geom_smooth(col = "red", fill = "red") + theme_bw()
}

# partial residuals vs age-> meets linearity assumption
viz_lin(motor.ph,"age")
# partial residuals vs slope-> meets linearity assumption
viz_lin(motor.ph,"slope")

#Assumption Tests for PH - Schoenfeld's residuals
motor.ph.zph <- cox.zph(motor.ph, transform = "identity")
motor.ph.zph
ggcoxzph(motor.ph.zph)




