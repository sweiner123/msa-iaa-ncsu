###################################
#                                 #
#        Financial Analytics:     #
#            Homework 1           #
#                                 #
#          Nikhil Suthar          #
#                                 #
###################################

library(gmodels)
library(vcd)
#install.packages("smbinning")
library(smbinning)
library(dplyr)
library(stringr)
#install.packages("shades")
library(shades)
library(latticeExtra)

accepted <- read.csv("accepted_customers.csv")
rejected <- read.csv("rejected_customers.csv")

# Change categorical variables to factors

factor <- c(
  "BUREAU", "CAR", "CARDS", "DIV", "EC_CARD", "FINLOAN", "LOCATION",
  "NAT", "PRODUCT", "PROF", "REGN", "RESID", "TEL", "NMBLOAN"
)

for (col in factors) {
  accepted[, col] <- as.factor(accepted[, col])
}

accepted$good <- abs(accepted$GB - 1)
table(accepted$good)

#make this example reproducible
# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepted)), size = floor(0.7*nrow(accepted)))

train <- accepted[train_id, ]
test <- accepted[-train_id, ]

#accepted$index <- 1:nrow(accepted)

# Split to training and test
# train <- sample_frac(accepted, 0.7)
# test <- anti_join(accepted, train, by ='index')

#Check missing values
sapply(train, function(x) sum(is.na(x)))

#####################
# SEPARATION CHECKS #
#####################

for (col in factors) {
  print(col)
  print(table(train[[col]], train$GB))

}

#BUREAU column
train$BUREAU <- as.character(train$BUREAU)
train$BUREAU[train$BUREAU == "2"] <- "2+"
train$BUREAU[train$BUREAU == "3"] <- "2+"

#CARDS
train$CARDS <- as.character(train$CARDS)
train$CARDS[train$CARDS == "American Express"] <- "Other credit car"
train$CARDS[train$CARDS == "VISA Others"] <- "VISA"
train$CARDS[train$CARDS == "VISA mybank"] <- "VISA"

#PRODUCT
train$PRODUCT <- as.character(train$PRODUCT)
train$PRODUCT[train$PRODUCT == ""] <- "Others"

#PROF
train$PROF <- as.character(train$PROF)
train$PROF[train$PROF == ""] <- "Others"

#TEL
train$TEL <- as.character(train$TEL)
train$TEL[train$TEL == "0"] <- "0 or 1"
train$TEL[train$TEL == "1"] <- "0 or 1"

for (col in factors) {
  train[, col] <- as.factor(train[, col])
}

# Perform smBinning on Continuous variables
age_bin <- smbinning(df = train, y = "good", x = "AGE")
age_bin$ivtable




## LOOP for Binning

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary # Only Continuous Variables >= 0.1 IV #

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(length(check_res) == 1) {     #This is to handle "NO Significant Splits"
    next
  }else if (check_res$iv) {
    if (check_res$iv < 0.1) {
      next
    } else {
      result_all_sig[[num_names[i]]] <- check_res
      }
    } else {
      next
    }
}

# Generating Variables of Bins and WOE Values #
for(i in 1:length(result_all_sig)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}


for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

##################
# MODEL BUILDING #
##################

# Build Initial Logistic Regression #
initial_score <- glm(data = train, GB ~ PERS_H_WOE +
                       AGE_WOE +
                       TMJOB1_WOE +
                       INCOME_WOE, 
                     weights = train$X_freq_, family = "binomial")

summary(initial_score)

####################
# MODEL EVALUATION #
####################

############# TRAINING DATA #############

# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)

# Overall Performance Metrics 
# -------------------------------------------------- 
#   KS : 0.2761 (Unpredictive)
# AUC : 0.6914 (Poor)
# 
# Classification Matrix 
# -------------------------------------------------- 
#   Cutoff (>=) : 0.0352 (Optimal)
# True Positives (TP) : 638
# False Positives (FP) : 349
# False Negatives (FN) : 411
# True Negatives (TN) : 702
# Total Positives (P) : 1049
# Total Negatives (N) : 1051
# 
# Business/Performance Metrics 
# -------------------------------------------------- 
#   %Records>=Cutoff : 0.4700
# Good Rate : 0.6464 (Vs 0.4995 Overall)
# Bad Rate : 0.3536 (Vs 0.5005 Overall)
# Accuracy (ACC) : 0.6381
# Sensitivity (TPR) : 0.6082
# False Neg. Rate (FNR) : 0.3918
# False Pos. Rate (FPR) : 0.3321
# Specificity (TNR) : 0.6679
# Precision (PPV) : 0.6464
# False Discovery Rate : 0.3536
# False Omision Rate : 0.3693
# Inv. Precision (NPV) : 0.6307
# 
# Note: 0 rows deleted due to missing data.

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")



############# TEST DATA #############

#Evaluate the Initial Model - Testing Data #
for(i in 1:length(result_all_sig)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 1)

# Overall Performance Metrics 
# -------------------------------------------------- 
#   KS : 0.3537 (Fair)
# AUC : 0.7144 (Fair)
# 
# Classification Matrix 
# -------------------------------------------------- 
#   Cutoff (>=) : 0.0363 (Optimal)
# True Positives (TP) : 273
# False Positives (FP) : 113
# False Negatives (FN) : 178
# True Negatives (TN) : 336
# Total Positives (P) : 451
# Total Negatives (N) : 449
# 
# Business/Performance Metrics 
# -------------------------------------------------- 
#   %Records>=Cutoff : 0.4289
# Good Rate : 0.7073 (Vs 0.5011 Overall)
# Bad Rate : 0.2927 (Vs 0.4989 Overall)
# Accuracy (ACC) : 0.6767
# Sensitivity (TPR) : 0.6053
# False Neg. Rate (FNR) : 0.3947
# False Pos. Rate (FPR) : 0.2517
# Specificity (TNR) : 0.7483
# Precision (PPV) : 0.7073
# False Discovery Rate : 0.2927
# False Omision Rate : 0.3463
# Inv. Precision (NPV) : 0.6537
# 
# Note: 0 rows deleted due to missing data.

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

# Train Data

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Train Scores", xlab = "Score")


# Test Data

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Scores", xlab = "Score")


# Plotting Default by Score in Train Data #
cutpoints <- quantile(train$Score, probs = seq(0,1,0.10))
train$Score.QBin <- cut(train$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.train <- round(table(train$Score.QBin, train$GB)[,2]/rowSums(table(train$Score.QBin, train$GB))*100,2)

print(Default.QBin.train)

Default.QBin.train.pop <- round(table(train$Score.QBin, train$GB)[,2]/(table(train$Score.QBin, train$GB)[,2] + table(train$Score.QBin, train$GB)[,1]*4.75)*100,2)

print(Default.QBin.train.pop)

# Plotting Default by Score in Test Data #
cutpoints <- quantile(test$Score, probs = seq(0,1,0.10))
test$Score.QBin <- cut(test$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.test <- round(table(test$Score.QBin, test$GB)[,2]/rowSums(table(test$Score.QBin, test$GB))*100,2)

print(Default.QBin.test)

barplot(Default.QBin.test, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,60),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 20.5, lwd = 2, lty = "dashed")
text(11.5, 23, "Current = 20.5%")

Default.QBin.test.pop <- round(table(test$Score.QBin, test$GB)[,2]/(table(test$Score.QBin, test$GB)[,2] + table(test$Score.QBin, test$GB)[,1]*4.75)*100,2)

print(Default.QBin.test.pop)

barplot(Default.QBin.test.pop, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,20),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 5.00, lwd = 2, lty = "dashed")
text(11.5, 6, "Current = 5.00%")



