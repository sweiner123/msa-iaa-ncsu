library(glmnet)
library(tidyverse)
library(AppliedPredictiveModeling)
data(FuelEconomy)

# Convert the variables to factors

cars2010$NumCyl <- as.factor(cars2010$NumCyl)
cars2010$AirAspirationMethod <- as.factor(cars2010$AirAspirationMethod)
cars2010$NumGears <- as.factor(cars2010$NumGears)
cars2010$TransLockup <- as.factor(cars2010$TransLockup)
cars2010$TransCreeperGear <- as.factor(cars2010$TransCreeperGear)
cars2010$IntakeValvePerCyl <- as.factor(cars2010$IntakeValvePerCyl)
cars2010$ExhaustValvesPerCyl <- as.factor(cars2010$ExhaustValvesPerCyl)
cars2010$VarValveTiming <- as.factor(cars2010$VarValveTiming)
cars2010$VarValveLift <- as.factor(cars2010$VarValveLift)


# Run a LASSO regression predicting the FE variable using all the remaining variables.
# Some of these predictor variables are coded as numeric, but should be treated as
# categorical.

set.seed(17)

train_x <- model.matrix(FE ~ ., data = cars2010)[, -1]
train_y <- cars2010$FE

cars_lasso <- glmnet(x = train_x,  y = train_y,  alpha = 1)
plot(cars_lasso, xvar = "lambda")


# Perform a CV LASSO to optimize the lambda value.
# a. What is the value of lambda that minimizes the MSE?
#  b. What is the value of lambda one standard error above the minimum MSE value?
#  c. How many variables are left at the penalty value that is one standard error
# above the minimum MSE value (think of variables as a whole, not per category)?
#  (HINT: Look at the coefficients from the model with coef function.)


cars_lasso_cv <- cv.glmnet(x = train_x,  y = train_y,  alpha = 1)

plot(cars_lasso_cv)

cars_lasso_cv$lambda.min 

# 0.001031258

cars_lasso_cv$lambda.1se

# 0.08969356

plot(cars_lasso_cv, xvar = "lambda")
abline(v = log(cars_lasso_cv$lambda.1se), col = "red", lty = "dashed")
abline(v = log(cars_lasso_cv$lambda.min), col = "black", lty = "dashed")

x = coef(cars_lasso, s = cars_lasso_cv$lambda.1se) %>%
  broom::tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Influential Variables") +
  xlab("Coefficient") +
  ylab(NULL)

#How many variables are left?
#13 variables
# Variables: 
# EngDispl, 
# NumCyl, 
# Transmission, 
# AirAspirationMethod, 
# NumGears, 
# TransLockup,
# TransCreeperGear,
# DriveDesc,
# IntakeValvePerCyl,
# ExhaustValvesPerCyl,
# CarlineClassDesc,
# VarValveTiming,
# VarValveLift

# MLR with p-value selection

cars_lr <- lm(FE ~ ., data = cars2010)
summary(cars_lr)

# F-statistic: 95.55 on 55 and 1051 DF,  p-value: < 2.2e-16
# The Global F Test proves that at least one variable is useful in predicting FE
# What % of variation is explained? R-Squared = 83.33%

summary(cars_lasso_cv)

