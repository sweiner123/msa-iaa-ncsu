## Logistic Homework 2

library(dplyr)
library(tidyverse)

binned_ins_data <- read.csv("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\02. Fall I\\Logistic Regression Class\\Homework 2\\Homework2_LR\\insurance_t_bin.csv")

#Check data types
str(binned_ins_data)

#Check missing values in all columns & count them
na_counts <- sapply(binned_ins_data, function(y) sum(length(which(is.na(y)))))

na_counts <- data.frame(na_counts)
subset(na_counts, na_counts > 0)

#na_counts
#INV         1075
#CC          1075
#CCPURC      1075
#HMOWN       1463

#Add a missing category 'M' to treat these missing values

binned_ins_data <- binned_ins_data %>%
  mutate(INV = ifelse(is.na(INV), "M", INV), 
         CC = ifelse(is.na(CC), "M", CC), 
         CCPURC = ifelse(is.na(CCPURC), "M", CCPURC), 
         HMOWN = ifelse(is.na(HMOWN), "M", HMOWN))

#check if any NA values
str(binned_ins_data)

#Convert all integers/numeric/chars to factors
binned_ins_data <-data.frame(lapply(binned_ins_data,factor))
#Check if conversion is successful
str(binned_ins_data)

#Check each variable for separation concerns

columns <- colnames(binned_ins_data)
for(i in 1:length(columns)){
  print(columns[i])
  print(table(binned_ins_data[[i]],binned_ins_data$INS))
}

#CASHBK, MMCRED have quasi-separation issues, fix them
binned_ins_data$CASHBK <- as.character(binned_ins_data$CASHBK)
binned_ins_data$CASHBK[which(binned_ins_data$CASHBK > 0)] <- "1+"
binned_ins_data$MMCRED <- as.character(binned_ins_data$MMCRED)
binned_ins_data$MMCRED[which(binned_ins_data$MMCRED > 2)] <- "3+"

#Check if issue is addressed
table(binned_ins_data$CASHBK, binned_ins_data$INS)
table(binned_ins_data$MMCRED, binned_ins_data$INS)

# Main effects model - Backward Selection

full_model <- glm(INS ~ ., data = binned_ins_data, family = binomial(link = "logit"))
  
back_model <- step(full_model, direction = "backward")

summary(back_model)
#INS ~ DDA + DIRDEP + NSF + SAV + IRA + LOC + INV + 
#  ILS + MM + MTG + CC + BRANCH + DDABAL_Bin + CHECKS_Bin + 
#  NSFAMT_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin

#Evaluate variables as a whole
library(car)
car::Anova(back_model, test = 'LR', type = 'III', singular.ok = TRUE)
