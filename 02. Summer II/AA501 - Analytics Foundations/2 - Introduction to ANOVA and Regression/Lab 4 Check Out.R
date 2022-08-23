
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(FuelEconomy)


#Q - 1

pairs(cars2010[,c('FE','EngDispl','NumCyl','ExhaustValvesPerCyl','VarValveTiming')])

cor(cars2010[,c('FE','EngDispl','NumCyl','ExhaustValvesPerCyl','VarValveTiming')])


#Q - 2 - ICECREAM DATASET

icream <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/icecream.csv", sep = " ")

slr <- lm(Sales ~ Temperature, data=icream)
par(mfrow=c(2,2))
plot(slr)

?lm


#Q - 3 MINNTEMP DATASET

minntemp <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/minntemp.csv", sep = " ")

slr_temp <- lm(Temp ~ Time, data=minntemp)
par(mfrow=c(2,2))
plot(slr_temp)
summary(slr_temp)

cor(minntemp$Temp, minntemp$Time)

?read.csv
