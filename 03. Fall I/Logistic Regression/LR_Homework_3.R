library(DescTools)
library(ggplot2)
library(survival)
library("InformationValue")
library("prediction")
library("ROCR")
#install.packages("caret")
library("caret")

##Reading csv file
ins_t_bin <- read.csv("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\02. Fall I\\Logistic Regression Class\\Homework 2\\Homework2_LR\\insurance_t_bin.csv")

keep <- c("INS", "DDA", "DIRDEP", "NSF", "SAV", "ATM", "CD", "IRA", "LOC", "ILS", "MM", "MTG", "SDB", 
          "MOVED", "INAREA", "HMOWN", "CC", "CCPURC", "INV", "CASHBK", "MMCRED", "RES", "BRANCH", 
          "ACCTAGE_Bin", "DDABAL_Bin", "DEPAMT_Bin", "CHECKS_Bin", "NSFAMT_Bin", "PHONE_Bin", 
          "TELLER_Bin", "SAVBAL_Bin", "ATMAMT_Bin", "POS_Bin", "POSAMT_Bin", "CDBAL_Bin", "IRABAL_Bin", 
          "LOCBAL_Bin", "INVBAL_Bin", "ILSBAL_Bin", "MMBAL_Bin", "MTGBAL_Bin", "CCBAL_Bin", "INCOME_Bin", 
          "LORES_Bin", "HMVAL_Bin", "AGE_Bin", "CRSCORE_Bin")
ins_t_bin <- subset(ins_t_bin, select = keep)

# Number of Missing Value per Variable #
sapply(ins_t_bin, function(x) sum(is.na(x)))

# Convert All Missing #
ins_t_bin$HMOWN[is.na(ins_t_bin$HMOWN)] <- 'M'
ins_t_bin$CC[is.na(ins_t_bin$CC)] <- 'M'
ins_t_bin$CCPURC[is.na(ins_t_bin$CCPURC)] <- 'M'
ins_t_bin$INV[is.na(ins_t_bin$INV)] <- 'M'

# Linear Separation Concerns #
LS <- NULL

for(i in 2:length(keep)){
  LS[i-1] <- sum(table(ins_t_bin$INS, ins_t_bin[[i]]) == 0)
}

data.frame(keep[-1], LS)

data.frame(keep[-1], LS)[order(-LS),]

# Fix CASHBK and MMCRED Variables #
ins_t_bin$CASHBK[which(ins_t_bin$CASHBK >= 1)] <- '1+'
ins_t_bin$MMCRED[which(ins_t_bin$MMCRED >= 3)] <- '3+'

# Backward Selection Logistic Regression Main Effects #
ins_t_bin[keep] <- lapply(ins_t_bin[keep], as.factor)

full.model <- glm(INS ~ ., data = ins_t_bin, family = binomial(link = "logit"))

back.model <- step(full.model, direction = "backward", k = log(length(ins_t_bin$INS)), trace = FALSE)
summary(back.model)

# Forward Selection to Obtain Interactions Model #
main.model <- glm(INS ~ (DDA + NSF + IRA + ILS + MM + MTG + CC + INV + DDABAL_Bin + 
                           CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin), 
                  data = ins_t_bin, family = binomial(link = "logit"))

int.model <- glm(INS ~ (DDA + NSF + IRA + ILS + MM + MTG + CC + INV + DDABAL_Bin + 
                          CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin)^2, 
                 data = ins_t_bin, family = binomial(link = "logit"))

for.model <- step(main.model,
                  scope = list(lower=formula(main.model), upper=formula(int.model)),
                  direction = "forward", k = log(length(ins_t_bin$INS)), trace = FALSE)
summary(for.model)

car::Anova(for.model, test = "LR", type = 'III', singular.ok = TRUE)

# Final Logistic Model #
final.model <- glm(INS ~ (DDA + NSF + IRA + ILS + MM + MTG + CC + INV + DDABAL_Bin + 
                            CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin +
                            DDA*IRA),
                   data = ins_t_bin, family = binomial(link = "logit"))
summary(final.model)

AIC(final.model)
#8814.716
PseudoR2(final.model, which = "Nagelkerke")
#0.3128844

ins_t_bin$p_hat <- predict(final.model, type = "response")
p1 <- ins_t_bin$p_hat[ins_t_bin$INS == 1]
p0 <- ins_t_bin$p_hat[ins_t_bin$INS == 0]

coef_discrim <- mean(p1) - mean(p0)
coef_discrim
#0.2430118

#Plot the coefficient of discrimination

ggplot(ins_t_bin, aes(p_hat, fill = factor(INS))) + 
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

#Concordance
Concordance(ins_t_bin$INS, ins_t_bin$p_hat)

#$Concordance
#[1] 0.7973362

# $Discordance
# [1] 0.2026638
# 
# $Tied
# [1] -2.775558e-17
# 
# $Pairs
# [1] 16273686

# Classification Metrics

confusionMatrix(ins_t_bin$INS, ins_t_bin$p_hat, threshold = 0.5)


plotROC(ins_t_bin$INS, ins_t_bin$p_hat)

ks_stat(ins_t_bin$INS, ins_t_bin$p_hat)

######################### OUR MODEL ###########################


#Our Model
our.model <- glm(INS ~ (SAVBAL_Bin + DDABAL_Bin + CDBAL_Bin + MM + ATMAMT_Bin + CHECKS_Bin +
                            TELLER_Bin + CC + DDA + INV + BRANCH + ILS + NSF + IRA +
                            DDA*IRA),
                   data = ins_t_bin, family = binomial(link = "logit"))
summary(our.model)

anova_ours <- car::Anova(our.model, test = 'LR', type = 'III', singular.ok = TRUE)
pval.for_ours <- data.frame(rownames(anova_ours),anova_ours$"Pr(>Chisq)")[order(anova_ours$"Pr(>Chisq)"),]


AIC(our.model)
# 8803.252
PseudoR2(our.model, which = "Nagelkerke")
# 0.3183384

ins_t_bin$p_hat_ours <- predict(our.model, type = "response")
p1_ours <- ins_t_bin$p_hat_ours[ins_t_bin$INS == 1]
p0_ours <- ins_t_bin$p_hat_ours[ins_t_bin$INS == 0]

coef_discrim_ours <- mean(p1_ours) - mean(p0_ours)
coef_discrim_ours
#0.2474811

ggplot(ins_t_bin, aes(p_hat_ours, fill = factor(INS))) + 
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

#Concordance
Concordance(ins_t_bin$INS, ins_t_bin$p_hat_ours)

# $Concordance
# [1] 0.8005162
# 
# $Discordance
# [1] 0.1994838
# 
# $Tied
# [1] -2.775558e-17
# 
# $Pairs
# [1] 16273686

# Classification Metrics

confusionMatrix(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = 0.5)

plotROC(ins_t_bin$INS, ins_t_bin$p_hat_ours)

ks_stat(ins_t_bin$INS, ins_t_bin$p_hat_ours)

## SENS, SPEC, & YOUDEN
sens <- NULL
spec <- NULL
youden <- NULL
cutoff <- NULL
for(i in 1:49){
  cutoff = c(cutoff, i/50)
  sens <- c(sens, sensitivity(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = i/50))
  spec <- c(spec, specificity(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = i/50))
  youden <- c(youden, youdensIndex(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = i/50))
}
ctable <- data.frame(cutoff, sens, spec, youden)
print(ctable[order(-youden),])

### PLOT ROC

pred <- ROCR::prediction(fitted(our.model), factor(ins_t_bin$INS))
perf <- performance(pred, measure = "tpr",
                    x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)


### PERFORMANCE

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]]
                                                  - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")

### SENS, SPEC, PRECISION, RECALL
sens <- NULL
spec <- NULL
youden <- NULL
cutoff <- NULL
reca <- NULL
prec <- NULL
f1<-NULL
for(i in 1:49){
  cutoff = c(cutoff, i/50)
  reca <- c(reca, sensitivity(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = i/50))
  prec <- c(prec, precision(ins_t_bin$INS, ins_t_bin$p_hat_ours, threshold = i/50))
  f1 <- c(f1, 2*((prec[i]*reca[i])/(prec[i] + reca[i])))
}
ctable <- data.frame(cutoff, reca, prec, f1)
print(ctable[order(-f1),])



############# VALIDATION DATA ################

ins_v_bin <- read.csv("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\02. Fall I\\Logistic Regression Class\\Homework 3\\Homework3_LR\\insurance_v_bin.csv")

# Number of Missing Value per Variable #
sapply(ins_v_bin, function(x) sum(is.na(x)))

# Convert All Missing #
ins_v_bin$HMOWN[is.na(ins_v_bin$HMOWN)] <- 'M'
ins_v_bin$CC[is.na(ins_v_bin$CC)] <- 'M'
ins_v_bin$CCPURC[is.na(ins_v_bin$CCPURC)] <- 'M'
ins_v_bin$INV[is.na(ins_v_bin$INV)] <- 'M'

#Convert all to factors
ins_v_bin <-data.frame(lapply(ins_v_bin,factor))



#valid_pred <- data.frame()
#valid_pred <- data.frame(valid_pred, 'Pred' = predict(final.model, newdata = ins_v_bin, type = "response"))

ins_v_bin$p_hat_ours_valid <- predict(our.model, newdata = ins_v_bin, type = "response")
p1_ours_valid <- ins_v_bin$p_hat_ours_valid[ins_v_bin$INS == 1]
p0_ours_valid <- ins_v_bin$p_hat_ours_valid[ins_v_bin$INS == 0]

coef_discrim_ours_valid <- mean(p1_ours_valid) - mean(p0_ours_valid)
coef_discrim_ours_valid
#0.2305472

ggplot(ins_v_bin, aes(p_hat_ours_valid, fill = factor(INS))) + 
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim, 3), sep = ""))

#Concordance
Concordance(ins_v_bin$INS, ins_v_bin$p_hat_ours_valid)

# $Concordance
# [1] 0.7819218
# 
# $Discordance
# [1] 0.2180782
# 
# $Tied
# [1] 0
# 
# $Pairs
# [1] 1025444

confusionMatrix(ins_v_bin$INS, ins_v_bin$p_hat_ours_valid, threshold = 0.295)

plotROC(ins_t_bin$INS, ins_t_bin$p_hat_ours)

ks_stat(ins_t_bin$INS, ins_t_bin$p_hat_ours)

### ACCURACY ON VALIDATION DATA

accuracy <- NULL
error <- NULL
cutoff <- NULL

for(i in 1:49){
  cutoff = c(cutoff, i/50)
  error <- c(error, misClassError(ins_v_bin$INS, ins_v_bin$p_hat_ours_valid, threshold = i/50))
  accuracy <- c(accuracy, (1 - misClassError(ins_v_bin$INS, ins_v_bin$p_hat_ours_valid, threshold = i/50)))
}

ctable <- data.frame(cutoff, error, accuracy)

print(ctable[order(-accuracy),])

## LIFT ON VALIDATION DATA


perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Training Data")
abline(h = 1, lty = 3)



confusionMatrix(factor(ins_v_bin$INS), factor(ins_v_bin$p_hat_ours_valid), threshold = 0.295)





