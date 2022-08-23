library(AppliedPredictiveModeling)
data(FuelEconomy)


cars2010$NumCyl <- as.factor(cars2010$NumCyl)
cars2010$AirAspirationMethod <- as.factor(cars2010$AirAspirationMethod)
cars2010$NumGears <- as.factor(cars2010$NumGears)
cars2010$TransLockup <- as.factor(cars2010$TransLockup)
cars2010$TransCreeperGear <- as.factor(cars2010$TransCreeperGear)
cars2010$IntakeValvePerCyl <- as.factor(cars2010$IntakeValvePerCyl)
cars2010$ExhaustValvesPerCyl <- as.factor(cars2010$ExhaustValvesPerCyl)
cars2010$VarValveTiming <- as.factor(cars2010$VarValveTiming)
cars2010$VarValveLift <- as.factor(cars2010$VarValveLift)

cars_lr <- lm(FE ~ EngDispl + NumCyl + Transmission + AirAspirationMethod + NumGears + TransLockup + TransCreeperGear + DriveDesc + IntakeValvePerCyl + ExhaustValvesPerCyl + CarlineClassDesc + VarValveTiming + VarValveLift, data = cars2010)
summary(cars_lr)

# F-statistic: 95.55 on 55 and 1051 DF,  p-value: < 2.2e-16
# The Global F Test proves that at least one variable is useful in predicting FE
# What % of variation is explained? R-Squared = 83.33%


#Q - 2

anova(cars_lr)
#Variable "VarValveTiming" has the highest p-value

#Q - 3
cars_lr_new <- lm(FE ~ EngDispl + NumCyl + Transmission + AirAspirationMethod + NumGears + TransLockup + TransCreeperGear + DriveDesc + IntakeValvePerCyl + ExhaustValvesPerCyl + CarlineClassDesc + VarValveLift, data = cars2010)
summary(cars_lr_new)

anova(cars_lr_new)

cars_lr_last <- lm(FE ~ EngDispl + NumCyl + Transmission + AirAspirationMethod + NumGears + TransLockup + TransCreeperGear + DriveDesc + IntakeValvePerCyl + CarlineClassDesc + VarValveLift, data = cars2010)
summary(cars_lr_last)

anova(cars_lr_last)

