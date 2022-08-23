library(AppliedPredictiveModeling)
data(FuelEconomy)
library(tidyverse)


cars2010$NumCyl <- as.factor(cars2010$NumCyl)
cars2010$AirAspirationMethod <- as.factor(cars2010$AirAspirationMethod)
cars2010$NumGears <- as.factor(cars2010$NumGears)
cars2010$TransLockup <- as.factor(cars2010$TransLockup)
cars2010$TransCreeperGear <- as.factor(cars2010$TransCreeperGear)
cars2010$IntakeValvePerCyl <- as.factor(cars2010$IntakeValvePerCyl)
cars2010$ExhaustValvesPerCyl <- as.factor(cars2010$ExhaustValvesPerCyl)
cars2010$VarValveTiming <- as.factor(cars2010$VarValveTiming)
cars2010$VarValveLift <- as.factor(cars2010$VarValveLift)

# Possible predictors are - EngDispl, NumCyl, Transmission, AirAspirationMethod, NumGears,
# TransLockup, TransCreeperGear, DriveDesc, IntakeValvePerCyl, ExhaustValvesPerCyl, CarlineClassDesc,
# VarValveTiming and VarValveLift.

train_sel = cars2010 %>%
  select("FE", 'EngDispl', 'NumCyl', 'Transmission', 'AirAspirationMethod', 'NumGears', 'TransLockup', 'TransCreeperGear', 'DriveDesc', 'IntakeValvePerCyl', 'ExhaustValvesPerCyl', 'CarlineClassDesc',
         'VarValveTiming', 'VarValveLift') %>%
  replace(is.na(.), 0)

# Q1 perform a forward selection (using the p-value criteria at 0.10)

# Create full model and empty model
full.model <- lm(FE ~ . , data = train_sel)
empty.model <- lm(FE ~ 1, data = train_sel)

# k = qchisq(alpha, 1, lower.tail = FALSE) for p-value with alpha selection
alpha.f=0.10
for.model_p <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "forward", k = qchisq(alpha.f, 1, lower.tail = FALSE)) 

# What is the final model?

# Step:  AIC=2731.08
# FE ~ EngDispl + CarlineClassDesc + DriveDesc + Transmission + 
# NumCyl + IntakeValvePerCyl + VarValveLift + TransCreeperGear + 
#  TransLockup

# What was the first variable added?
# -> EngDispl

# What was the last variable added?
# -> TransLockup

# Q2 How many variables (not parameters or estimates) would result in the final model from
# using stepwise selection with the BIC criteria?

# Create full model and empty model
full.model <- lm(FE ~ . , data = train_sel)
empty.model <- lm(FE ~ 1, data = train_sel)

# k = log(n) for BIC selection
for.model_bic <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = log(nrow(train_sel)))

# 7 variables are selected in the final model
# Step:  AIC=2904.37
# FE ~ EngDispl + DriveDesc + CarlineClassDesc + NumCyl + Transmission + 
#  IntakeValvePerCyl + VarValveLift


# Are the two models from a and b the same?
# No, they are not the same. One uses forward selection and the other uses stepwise selection.
# Forward selection is better as per the AIC value.





