library(ggplot2)
library(tidyverse)
library(InformationValue)

# Read in the safety data
safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

# Build the initial logistic regression model
safe_logit <- glm(Unsafe ~ Region + Weight + factor(Size), data = safety,
                  family = binomial(link = "logit"))

summary(safe_logit)

Concordance(safety$Unsafe, predict(safe_logit, type = "response"))

# Build the reduced model
safe_logit2 <- glm(Unsafe ~ factor(Size), data = safety,
                  family = binomial(link = "logit"))

summary(safe_logit2)

Concordance(safety$Unsafe, predict(safe_logit2, type = "response"))
