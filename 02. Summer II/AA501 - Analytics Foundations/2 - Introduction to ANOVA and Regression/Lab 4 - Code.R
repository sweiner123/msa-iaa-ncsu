
## Lab 4 - Simple Linear Regression

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(FuelEconomy)

pairs(cars2010[,c('FE','EngDispl','NumCyl','ExhaustValvesPerCyl','VarValveTiming')])

pairs(cars2010[,c('EngDispl','NumCyl','ExhaustValvesPerCyl','VarValveTiming')])


slr_fuel <- lm(FE ~ EngDispl, data=cars2010)
summary(slr_fuel)

#EQUATION = 50.5632 - 4.5209X