###################################
#                                 #
#         Machine Learning:       #
#            Homework 1           #
#                                 #
#          Nikhil Suthar          #
#                                 #
###################################

library(tidyverse)
#install.packages("earth")
library(earth)
library(InformationValue)
library(ROCR)
library(mgcv)


#Read the training data
ins_t <- read.csv("insurance_t.csv")

# Number of Missing Value per Variable #
sapply(ins_t, function(x) sum(is.na(x)))

#Median Imputation of missing values
ins_t_imp <- ins_t %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# Check if the imputation worked
sapply(ins_t_imp, function(x) sum(is.na(x)))

#Find columns to be converted to factors -> binary indicators + Ordinal Variables

ind_vars <- c("DDA", "DIRDEP", "NSF", "SAV", "ATM", "CD", "IRA", "INV", "MM", "MMCRED",
              "CC", "CCPURC", "SDB", "INAREA", "INS")

#Convert them to factors
ins_t_imp[,ind_vars] <- lapply(ins_t_imp[,ind_vars] , factor)

str(ins_t_imp)

# EARTH/MARS on all variables
mars2 <- earth(INS ~ ., data = ins_t_imp,  glm=list(family=binomial))
summary(mars2)

# Variable importance metric
evimp(mars2)

##P-hat Values
ins_t_imp$p_hat <- predict(mars2, type = "response")[,1]
p1 <- ins_t_imp$p_hat[ins_t_imp$INS == 1]
p0 <- ins_t_imp$p_hat[ins_t_imp$INS == 0]

##Discrimination Slope
coef_discrim <- mean(p1) - mean(p0)
ggplot(ins_t_imp, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

##Rank-Order Statistics
Concordance(ins_t_imp$INS, ins_t_imp$p_hat)

##ROC Curve
pred <- prediction(ins_t_imp$p_hat, ins_t_imp$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins_t_imp$INS, ins_t_imp$p_hat)
#0.8007509

############################ CROSS VALIDATION ################################

#Drop p_hat
ins_t_imp = subset(ins_t_imp, select = -c(p_hat))

# EARTH/MARS on all variables and using Cross Validation
mars_cv <- earth(INS ~ ., data = ins_t_imp,  glm=list(family=binomial), pmethod = "cv", nfold = 10, trace = .5)
summary(mars_cv)

evimp(mars_cv)

##P-hat Values
ins_t_imp$p_hat <- predict(mars_cv, type = "response")[,1]
p1 <- ins_t_imp$p_hat[ins_t_imp$INS == 1]
p0 <- ins_t_imp$p_hat[ins_t_imp$INS == 0]

##Discrimination Slope
coef_discrim <- mean(p1) - mean(p0)
ggplot(ins_t_imp, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

##Rank-Order Statistics
Concordance(ins_t_imp$INS, ins_t_imp$p_hat)

##ROC Curve
pred <- prediction(ins_t_imp$p_hat, ins_t_imp$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins_t_imp$INS, ins_t_imp$p_hat)
#0.8009261

#### Use only a subset of variables #####################

#Drop p_hat
ins_t_imp = subset(ins_t_imp, select = -c(p_hat))

full.model <- glm(INS ~ ., data = ins_t_imp, family = binomial(link = "logit"))

back.model <- step(full.model, direction = "backward", k = log(length(ins_t_imp$INS)), trace = FALSE)
summary(back.model)

#Selected Variables by Backward selection
#INS ~ ACCTAGE + DDA + DDABAL + DEP + CHECKS + TELLER + SAV + 
#  SAVBAL + ATMAMT + CD + CDBAL + IRA + INV + MM + CC

# EARTH/MARS on a subset of variables
mars3 <- earth(INS ~ (ACCTAGE + DDA + DDABAL + DEP + CHECKS + TELLER + SAV + 
                 SAVBAL + ATMAMT + CD + CDBAL + IRA + INV + MM + CC), data = ins_t_imp,  glm=list(family=binomial))
summary(mars3)

# Variable importance metric
evimp(mars3)

##P-hat Values
ins_t_imp$p_hat <- predict(mars3, type = "response")[,1]
p1 <- ins_t_imp$p_hat[ins_t_imp$INS == 1]
p0 <- ins_t_imp$p_hat[ins_t_imp$INS == 0]

##Discrimination Slope
coef_discrim <- mean(p1) - mean(p0)
ggplot(ins_t_imp, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

##Rank-Order Statistics
Concordance(ins_t_imp$INS, ins_t_imp$p_hat)

##ROC Curve
pred <- prediction(ins_t_imp$p_hat, ins_t_imp$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins_t_imp$INS, ins_t_imp$p_hat)
#0.7941578

######################## GAM Model #############################

#Drop p_hat
ins_t_imp = subset(ins_t_imp, select = -c(p_hat))

# GAM's on all variables
gam2 <- mgcv::gam(INS ~ (s(ACCTAGE) + # The k = option sets max for EDF. Might need to use with variables with only a few unique values (< 15)
                    s(DDABAL) +
                    s(DEP) +
                    s(DEPAMT) +
                    s(CHECKS) + 
                    s(NSFAMT) +
                    s(PHONE) +
                    s(TELLER) +
                    s(SAVBAL) +
                    s(ATMAMT) +
                    s(POS) +
                    s(POSAMT) +
                    s(CDBAL) +
                    s(IRABAL) +
                    s(INVBAL) +
                    s(MMBAL) +
                    s(CCBAL) +
                    s(INCOME) +
                    s(LORES) +
                    s(HMVAL) +
                    s(AGE) +
                    s(CRSCORE) +
                    DDA +
                    DIRDEP +
                    NSF + 
                    SAV +
                    ATM +
                    CD +
                    IRA +
                    INV + 
                    MM +
                    factor(MMCRED) +
                    CC +
                    factor(CCPURC) +
                    SDB +
                    INAREA +
                    BRANCH), method = 'REML', select = TRUE, data = ins_t_imp, family = 'binomial')
summary(gam2)

# Remaining variables after selection
# Tried with and without NSF, DEP, BRANCH, ATM

gam3 <- mgcv::gam(INS ~ (s(ACCTAGE) + 
                           s(DDABAL) +
                        
                           s(CHECKS) + 
                           s(TELLER) +
                           s(SAVBAL) +
                           s(ATMAMT) +
                           s(CDBAL) +
                           DDA +
                          
                          
                           CD +
                           IRA +
                           INV + 
                           MM +
                           CC), method = 'REML', select = TRUE,data = ins_t_imp, family = 'binomial')

summary(gam3)


##P-hat Values

ins_t_imp$p_hat <- as.vector(mgcv::predict.gam(gam3, type = "response"))
p1 <- ins_t_imp$p_hat[ins_t_imp$INS == 1]
p0 <- ins_t_imp$p_hat[ins_t_imp$INS == 0]

##Discrimination Slope
coef_discrim <- mean(p1) - mean(p0)
ggplot(ins_t_imp, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

##Rank-Order Statistics
Concordance(ins_t_imp$INS, ins_t_imp$p_hat)

##ROC Curve
pred <- ROCR::prediction(ins_t_imp$p_hat, ins_t_imp$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins_t_imp$INS, ins_t_imp$p_hat)
#0.7998301

