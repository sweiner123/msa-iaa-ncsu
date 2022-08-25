
# Lab 9

library(AppliedPredictiveModeling)
data(FuelEconomy)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(tidyverse)


cars_lr <- lm(FE ~ EngDispl + 
                  Transmission + 
                  AirAspirationMethod + 
                  TransLockup + 
                  TransCreeperGear + 
                  DriveDesc + 
                  IntakeValvePerCyl + 
                  CarlineClassDesc + 
                  VarValveLift, data = cars2010)


b = ggplot(cars_lr,aes(x=n.index,y=rstudent(cars_lr)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
summary(cars_lr)

dwtest(cars_lr,alternative="greater")

# we would reject the null hypothesis and conclude there appears to be 
# significant positive autocorrelation

n = nrow(cars2010)
n.index = seq(1,nrow(cars2010))
##Plots of outliers
a = ggplot(cars_lr,aes(x=n.index,y=rstandard(cars_lr)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "Internal Studentized Residuals",x="Observation",y="Residuals")


##Influential points
c = ggplot(lm.model,aes(x=n.index,y=rstandard(lm.model)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "Internal Studentized Residuals",x="Observation",y="Residuals")

##Cook's D
D.cut=4/(nrow(cars2010)-9-1)

d =ggplot(cars_lr,aes(x=n.index,y=cooks.distance(cars_lr)))+geom_point(color="orange")+geom_line(y=D.cut)+labs(title = "Cook's D",x="Observation",y="Cook's Distance")

cooks <- cooks.distance(cars_lr)
cooks_frame <- as.data.frame(cooks)
cooks_frame2 <- cbind(cooks_frame,n.index)

cooks_frame3 <- cooks_frame2 %>% arrange(desc(cooks))

##Dffit
df.cut=2*(sqrt((9)/nrow(cars2010)))

e =ggplot(cars_lr,aes(x=n.index,y=dffits(cars_lr)))+geom_point(color="orange")+geom_line(y=df.cut)+geom_line(y=-df.cut)+labs(title = "DFFITS",x="Observation",y="DFFITS")

db.cut=2/sqrt(nrow(races.table))

#Studentized Residual

stud_resid <- rstudent(cars_lr)
stud_frame <- as.data.frame(stud_resid)
stud_frame2 <- cbind(stud_frame,n.index)

cooks_frame3 <- stud_frame2 %>% filter(abs(stud_resid)>3)

sum(rstudent(cars_lr) >3)
sum(rstudent(cars_lr) < (-3))

