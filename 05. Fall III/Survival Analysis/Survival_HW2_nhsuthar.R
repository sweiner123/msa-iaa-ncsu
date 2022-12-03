###################################
#                                 #
#         Survival Analysis:      #
#            Homework 2           #
#                                 #
#          Nikhil Suthar          #
#                                 #
###################################


# Import libraries
library(tidyverse)
library(survival)
#install.packages("survminer")
library(survminer)
#install.packages("flexsurv")
library(flexsurv)
library(car)

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# Failed pump percentage
table(hurricane$reason)*100/nrow(hurricane)

# 0 – no failure
# 1 – flood failure
# 2 – motor failure
# 3 – surge failure
# 4 – jammed failure

# Creating a failure variable for flood vs no flood
hurricane$failure <- ifelse(hurricane$reason == 1, 1, 0)

table(hurricane$failure)

############################ Part 1 #######################

# Accelerated Failure Time Model 

#Check which distribution will fit the data using grahical method

#Checking Weibull Distributions
hurr_aft.w <- flexsurvreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "weibull")

plot(hurr_aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

#Checking Exponential Distributions
hurr_aft.exp <- flexsurvreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "exp")

plot(hurr_aft.exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")

#Checking Gamma Distributions
hurr_aft.g <- flexsurvreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "gamma")

plot(hurr_aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

#Checking Log-Logistic Distributions
hurr_aft.ll <- flexsurvreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "llogis")

plot(hurr_aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Log Logistic Distribution")

#Checking Log-Normal Distributions
hurr_aft.ln <- flexsurvreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "lnorm")

plot(hurr_aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Log Normal Distribution")

#Get Log-Likelihoods for all
lik_Exp <- hurr_aft.exp$loglik
lik_Wei <- hurr_aft.w$loglik
lik_Gam <- hurr_aft.g$loglik
lik_LogL <- hurr_aft.ll$loglik
lik_LogNorm <- hurr_aft.ln$loglik

lik_Exp
lik_Wei
lik_Gam
lik_LogL
lik_LogNorm

#Log-Logistic has the highest Likelihood value

## Comparing Distributions

p_val_Exp_Gam = pchisq((-2*(lik_Exp-lik_Gam)), 2,lower.tail=F)
p_val_Wei_Gam = pchisq((-2*(lik_Wei-lik_Gam)), 1,lower.tail=F)
p_val_Gam_LogL = pchisq((-2*(lik_Gam-lik_LogL)), 1,lower.tail=F)
p_val_Gam_LogNorm = pchisq((-2*(lik_Gam-lik_LogNorm)), 1,lower.tail=F)

Tests = c('Exp v/s Gamma', 'Weibull v/s Gamma', 'Gamma v/s Log Logistic', 'Gamma v/s Log Normal')

P_values = c(p_val_Exp_Gam, p_val_Wei_Gam, p_val_Gam_LogL, p_val_Gam_LogNorm)
temp_df <- cbind(Tests, P_values)

write.csv(temp_df,"p_values.csv")

# Perform variable selection once you have your optimal distribution

#Start with full model using main effects, alpha = 0.03
llogis_1 <- survreg(Surv(hour, failure) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = 'loglogistic')
summary(llogis_1)

#remove age

llogis_2 <- survreg(Surv(hour, failure) ~ backup + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = 'loglogistic')
summary(llogis_2)

#remove elevation

llogis_3 <- survreg(Surv(hour, failure) ~ backup + bridgecrane + servo + gear + trashrack + slope, data = hurricane, dist = 'loglogistic')
summary(llogis_3)

#remove bridgecrane

llogis_4 <- survreg(Surv(hour, failure) ~ backup + servo + gear + trashrack + slope, data = hurricane, dist = 'loglogistic')
summary(llogis_4)

#remove gear

llogis_5 <- survreg(Surv(hour, failure) ~ backup + servo + trashrack + slope, data = hurricane, dist = 'loglogistic')
summary(llogis_5)

#remove trashrack

llogis_6 <- survreg(Surv(hour, failure) ~ backup + servo + slope, data = hurricane, dist = 'loglogistic')
summary(llogis_6)

#remove backup

llogis_7 <- survreg(Surv(hour, failure) ~ servo + slope, data = hurricane, dist = 'loglogistic')
summary(llogis_7)

#This is the final model

# Effect of the slope variable (in %)
100 * (exp(llogis_7$coefficients["slope"])-1)

# Effect of the servo variable (in %)
100 * (exp(llogis_7$coefficients["servo"])-1)

#Include a table of significant variables ranked by p-value.
#Interpret the effects of the most significant variable.

final_llogis <- flexsurvreg(Surv(hour, failure) ~ servo + slope, data = hurricane, dist = 'llogis')
summary(final_llogis)

plot(final_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Log Logistic Distribution")

#Parameter interpretation
(exp(coef(final_llogis))-1)*100

############################ Part 2 #############################


# Analyze the pumps that failed due to flood failure and evaluate which pumps you would
# recommend getting an upgrade on the factors you found significant in your model.
# You can only perform one upgrade per pump.
# You have a fixed budget of $2.5 million and must use this money accordingly to keep
# pumps functioning as long as possible.
# Provide a list of the pumps as well as the upgrades for those pumps.
# For each pump estimate the time benefit for your upgrade.
# (HINT: Use a similar approach as we did in class where we analyzed the benefit of
#   giving financial aid to prisoners.)

# Prob. of survival at 48 hours.
survprob_48h <- 1 - psurvreg(48,
                             mean = predict(llogis_7, type = "lp"),
                             scale = llogis_7$scale,
                             distribution = llogis_7$dist)

#Had they been given the upgrade?
survprob_48h_upgrade <- 1 - psurvreg(48,
                                     mean = predict(llogis_7, type = "lp") +
                                       coef(llogis_7)['servo'],
                                     scale = llogis_7$scale,
                                     distribution = llogis_7$dist)


survprob_48_df <- as.data.frame(cbind(c(1:length(survprob_48h)), survprob_48h, survprob_48h_upgrade))
colnames(survprob_48_df) <- c('pump_number','prob_surv', 'prob_surv_upgrade')

# Add results to the dataset
hurricane$surv_prob48 <- survprob_48h
hurricane$surv_prob48_up <- survprob_48h_upgrade
hurricane$surv_prob48_diff <- hurricane$surv_prob48_up - hurricane$surv_prob48

# filter the ones that does not have SERVO upgrade & failed because of flood.
hurricane %>%
  dplyr::filter(servo == 0, failure == 1) %>%
  dplyr::select(servo, failure, surv_prob48, surv_prob48_up, surv_prob48_diff) %>% 
  dplyr::arrange(surv_prob48_diff) %>% 
  head(16)


# df impact_upgrade
impact_upgrade = data.frame(c(1:nrow(hurricane)),
                            hurricane[,c("failure","servo",
                                  "surv_prob48","surv_prob48_up","surv_prob48_diff")])
colnames(impact_upgrade) = c('pump_number', 'Flood_Fail', 'Servo', 
                             'surv_prob48', 'surv_prob48_up', 'surv_prob48_diff')

# decide which the pumps to upgrade - filter the ones that failed and did not have the servo upgrade
# arrange by descending order of increase in survival prob @ 48H & prioritize those failed due to Flood
impact_upgrade_fin = impact_upgrade %>%
  dplyr::filter(Servo == 0, Flood_Fail == 1) %>%
  arrange(desc(surv_prob48_diff), desc(Flood_Fail))

#Budget is 2.5Million dollars, every servo upgrade costs $150K, so we can upgrade 16 pumps total

n_pump_upgrade =floor(2.5/0.15)
impact_upgrade_fin$upgrade <- c(rep(1,n_pump_upgrade),
                                rep(0,nrow(impact_upgrade_fin)-n_pump_upgrade))

# pumps number
pump_upgrade <- impact_upgrade_fin %>% dplyr::filter(upgrade == 1) %>% select(pump_number, Servo, surv_prob48, surv_prob48_up, surv_prob48_diff) %>% arrange(pump_number)
write.csv(pump_upgrade, "pump_upgrades.csv")

# avg increase in survival probability at 48hrs of the 16 upgraded pumps
mean_survprob_org <- mean(impact_upgrade_fin[impact_upgrade_fin$upgrade == 1,]$surv_prob48) # 0.5912938
mean_survprob_up <-mean(impact_upgrade_fin[impact_upgrade_fin$upgrade == 1,]$surv_prob48_up) # 0.7349276
mean_survprob_diff <- mean(impact_upgrade_fin[impact_upgrade_fin$upgrade == 1,]$surv_prob48_diff) # 0.1436337

mean_survprob_org
mean_survprob_up
mean_survprob_diff

# in percentage
round(mean_survprob_org, 3) * 100 #59.1
round(mean_survprob_up, 3) * 100 #73.5
round(mean_survprob_diff, 3) * 100 #14.4



######## BACKUP ############

# Prob. of survival at 48 hours.
survprob_48h_back <- 1 - psurvreg(48,
                             mean = predict(llogis_6, type = "lp"),
                             scale = llogis_6$scale,
                             distribution = llogis_6$dist)

#What if they been given the upgrade?
survprob_48h_upgrade_back <- 1 - psurvreg(48,
                                     mean = predict(llogis_6, type = "lp") +
                                       coef(llogis_6)['backup'],
                                     scale = llogis_6$scale,
                                     distribution = llogis_6$dist)


survprob_48_df_back <- as.data.frame(cbind(c(1:length(survprob_48h_back)), survprob_48h_back, survprob_48h_upgrade_back))
colnames(survprob_48_df_back) <- c('pump_number','prob_surv', 'prob_surv_upgrade')

# Add results to the dataset

backup <- hurricane

backup$surv_prob48 <- survprob_48h_back
backup$surv_prob48_up <- survprob_48h_upgrade_back
backup$surv_prob48_diff <- backup$surv_prob48_up - backup$surv_prob48

# filter the ones that does not have BACKUP upgrade & failed because of flood.
backup %>%
  dplyr::filter(backup == 0, failure == 1) %>%
  dplyr::select(backup, failure, surv_prob48, surv_prob48_up, surv_prob48_diff) %>% 
  dplyr::arrange(surv_prob48_diff) %>% 
  head(16)


# df impact_upgrade
impact_upgrade_backup = data.frame(c(1:nrow(backup)),
                                   backup[,c("failure","backup",
                                         "surv_prob48","surv_prob48_up","surv_prob48_diff")])
colnames(impact_upgrade_backup) = c('pump_number', 'Flood_Fail', 'Backup', 
                             'surv_prob48', 'surv_prob48_up', 'surv_prob48_diff')

# decide which the pumps to upgrade - filter the ones that failed and did not have the servo upgrade
# arrange by descending order of increase in survival prob @ 48H & prioritize those failed due to Flood
impact_upgrade_fin_backup = impact_upgrade_backup %>%
  dplyr::filter(Backup == 0, Flood_Fail == 1) %>%
  arrange(desc(surv_prob48_diff), desc(Flood_Fail))

#Budget is 2.5Million dollars, every backup upgrade costs $100K, so we can upgrade 25 pumps total

n_pump_upgrade =floor(2.5/0.1)
impact_upgrade_fin_backup$upgrade <- c(rep(1,n_pump_upgrade),
                                rep(0,nrow(impact_upgrade_fin_backup)-n_pump_upgrade))

# pumps number
pump_upgrade <- impact_upgrade_fin_backup %>% dplyr::filter(upgrade == 1) %>% select(pump_number, Backup, Flood_Fail) %>% arrange(pump_number)
print(pump_upgrade)

# avg increase in survival probability at 48hrs of the 25 upgraded pumps
mean_survprob_org_backup <- mean(impact_upgrade_fin_backup[impact_upgrade_fin_backup$upgrade == 1,]$surv_prob48) # 0.5912938
mean_survprob_up_backup <-mean(impact_upgrade_fin_backup[impact_upgrade_fin_backup$upgrade == 1,]$surv_prob48_up) # 0.7349276
mean_survprob_diff_backup <- mean(impact_upgrade_fin_backup[impact_upgrade_fin_backup$upgrade == 1,]$surv_prob48_diff) # 0.1436337

mean_survprob_org_backup
mean_survprob_up_backup
mean_survprob_diff_backup

# in percentage
round(mean_survprob_org_backup, 3) * 100 #62.3
round(mean_survprob_up_backup, 3) * 100 #71.6
round(mean_survprob_diff_backup, 3) * 100 #9.3











