
# Lab 13

library(ggplot2)
library(gmodels)

safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

# Unsafe: binary safety designation 
# (1 = below average (unsafe), 0 = average or above average (safe))
# Type: type of car (Large, Medium, Small, Sport/Utility, Sports)
# Region: manufacturing region (Asia, N America)
# Weight: integer value for car weight ranging from 1 to 6
# Size: size of car corresponding to Type 
# (1 = Small/Sports, 2 = Medium, 3 = Large or Sport/Utility)

# Q. 1a)

# a. Build a logistic regression, predicting Unsafe using the variables Region, Weight, and
# Size. Treat Weight as a continuous variable. Treat Region and Size as categorical. Make
# sure to use the factor function for Size.
# a. Which variables were significant at the 0.05 level?
#   b. What is the concordance proportion for this model?

safety$Size <- as.factor(safety$Size)

safety_logit <- glm(Unsafe ~ Region + Weight + Size,
                  data = safety, family = binomial(link = "logit"))

summary(safety_logit)

100*(exp(cbind(coef(safety_logit), confint(safety_logit)))-1)

library(InformationValue)
Concordance(safety$Unsafe, predict(safety_logit, type = "response"))

# $Concordance
# [1] 0.8191919
# 
# $Discordance
# [1] 0.1808081
# 
# $Tied
# [1] 5.551115e-17

# Keep ALL Variables

full.model <- glm(Unsafe ~ Region + Weight + Size , 
                  data = safety,family = binomial(link = "logit"))

empty.model <- glm(Unsafe ~ Region + Weight + Size , 
                   data = safety, family = binomial(link = "logit"))

back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward", k = log(dim(safety)[1]))

summary(back.model)

Concordance(safety$Unsafe, predict(back.model, type = "response"))

# Remove Region

full.model <- glm(Unsafe ~ Weight + Size , 
                  data = safety,family = binomial(link = "logit"))

empty.model <- glm(Unsafe ~ Weight + Size , 
                   data = safety, family = binomial(link = "logit"))

back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward", k = log(dim(safety)[1]))

summary(back.model)

Concordance(safety$Unsafe, predict(back.model, type = "response"))

# Remove Weight

full.model <- glm(Unsafe ~ Size , 
                  data = safety,family = binomial(link = "logit"))

empty.model <- glm(Unsafe ~ Size , 
                   data = safety, family = binomial(link = "logit"))

back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward", k = log(dim(safety)[1]))

summary(back.model)

Concordance(safety$Unsafe, predict(back.model, type = "response"))

100*(exp(cbind(coef(back.model), confint(back.model)))-1)

# What is the interpretation of the Size variable for comparing categories 1 to 3?
