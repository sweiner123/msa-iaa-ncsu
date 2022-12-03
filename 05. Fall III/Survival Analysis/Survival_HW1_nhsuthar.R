###################################
#                                 #
#         Survival Analysis:     #
#            Homework 1           #
#                                 #
#          Nikhil Suthar          #
#                                 #
###################################

# Import libraries
library(tidyverse)
library(survival)
#install.packages("survminer")
library(survminer)

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# Number of Missing Value per Variable #
sapply(hurricane, function(x) sum(is.na(x)))

# Failed pump percentage
table(hurricane$reason)*100/nrow(hurricane)

hurricane %>% group_by(reason) %>% summarise(avg_time_until_failure = mean(hour))

hurricane$failure <- 1-hurricane$survive
#1 = pump failed, it did not survive
#0 = pump survived, it did not fail

model1 <- survfit(Surv(hour, failure) ~ reason, data = hurricane)

ggsurvplot(model1, data = df, conf.int = T,
           legend.title = "Failure by Reason", xlab = "Time (in Hours)",
           ylab = "Survival Probability", break.y.by = 0.20)


# Hazard Probabilities

probs = data.frame(data = matrix(nrow = 48, ncol = 0))

for (i in seq(1:4)) {
  curr_df <- hurricane %>% filter(reason == i)
  curr_model <- survfit(Surv(hour, failure)~reason, data = curr_df)
  curr_index <- rep(0, length=max(model1$strata))
  curr_hazard <- curr_model$n.event/curr_model$n.risk
  curr_index[(curr_model$time)]= curr_hazard
  extra_zeros <- rep(0, 48-length(curr_index))
  curr_index <- append(curr_index, extra_zeros)
  probs = cbind(probs, curr_index)
}

colnames(probs) <- c("Reason1", "Reason2", "Reason3", "Reason4")
probs <- rbind(probs, c("type1"=0, "type2"=0, "type3"=0, "type4"=0))

df2 <- cbind(seq(1:49), probs)
colnames(df2) <- c("index","Reason1", "Reason2", "Reason3", "Reason4")

df2 %>% ggplot(aes(x=index)) +
  geom_line(aes(y=Reason1, color='Reason1'), size = 0.5) +
  geom_line(aes(y=Reason2, color='Reason2'), size = 0.5) +
  geom_line(aes(y=Reason2, color='Reason3'), size = 0.5) +
  geom_line(aes(y=Reason4, color='Reason4'), size = 0.5) +
  scale_color_manual(name = "Failure Reason", values = c("Reason1" = "red", "Reason2" = "blue", "Reason3" = "black", "Reason4" = "cyan")) +
  labs(x='Time (in Hours) ', y="Conditional Failure Probabilities",
       title = "Failure Probability by Failure Type")


survdiff(Surv(hour, failure)~reason, data=hurricane, rho=0)
