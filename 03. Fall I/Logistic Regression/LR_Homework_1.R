library(tidyverse)
library(ggplot2)
library(mgcv)

setwd("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\02. Fall I\\Logistic Regression Class\\Homework 1\\Homework1_LR")

# Read the training data

train <- read.csv("insurance_t.csv")

sum(is.na(train$ACCTAGE))

table(train$INS)

ggplot(data = train) +
  geom_bar(mapping = 
             aes(x = INS))


library(DescTools)


train$DDA <- as.factor(train$DDA)
OddsRatio(table(train$DDA, train$INS))

train$DIRDEP <- as.factor(train$DIRDEP)
OddsRatio(table(train$DIRDEP, train$INS))

train$NSF <- as.factor(train$NSF)
OddsRatio(table(train$NSF, train$INS))

train$CASHBK <- as.factor(train$CASHBK)

library(vcd)
library(vcdExtra)

assocstats(table(train$DDA, train$INS))

#DDA
chisq.test(table(train$DDA, train$INS))
CMHtest(table(train$DDA, train$INS))$table[1,]

#DIRDEP
chisq.test(table(train$DIRDEP, train$INS))
CMHtest(table(train$DIRDEP, train$INS))$table[1,]

#NSF
chisq.test(table(train$NSF, train$INS))
CMHtest(table(train$NSF, train$INS))$table[1,]

#CASHBK
cor.test(x = as.numeric(ordered(train$CASHBK)),
         y = as.numeric(ordered(train$INS)),
         method = "spearman")

#CCPURC
cor.test(x = as.numeric(ordered(train$CCPURC)),
         y = as.numeric(ordered(train$INS)),
         method = "spearman")

#ACCTAGE

logit.model <- glm(INS ~ ACCTAGE, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')
#Getting in error in ANOVA - why?

#Check linearity assumption
fit.gam <- gam(INS ~ s(ACCTAGE),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")


#DDABAL

logit.model <- glm(INS ~ DDABAL, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')

#Check linearity assumption
fit.gam <- gam(INS ~ s(DDABAL),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")

#DEP

logit.model <- glm(INS ~ DEP, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')

#Check linearity assumption
fit.gam <- gam(INS ~ s(DEP),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")

#DEPAMT

logit.model <- glm(INS ~ DEPAMT, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')

#Check linearity assumption
fit.gam <- gam(INS ~ s(DEPAMT),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")

#CHECKS

logit.model <- glm(INS ~ CHECKS, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')

#Check linearity assumption
fit.gam <- gam(INS ~ s(CHECKS),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")

#NSFAMT

logit.model <- glm(INS ~ NSFAMT, 
                   data = train, family = binomial(link = "logit"))
summary(logit.model)

exp(cbind(coef(logit.model), confint(logit.model)))

#LRT Test
logit.model.r <- glm(INS ~ 1, 
                     data = train, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')

#Check linearity assumption
fit.gam <- gam(INS ~ s(NSFAMT),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
anova(logit.model, fit.gam, test="Chisq")

