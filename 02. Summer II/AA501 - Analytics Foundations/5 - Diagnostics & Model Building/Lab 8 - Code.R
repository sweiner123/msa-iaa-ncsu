library(ggplot2)

# Lab 8

cafe <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cafeteria.csv")

lr_cafe <- lm(Sales~Dispensers, data = cafe)
plot(lr_cafe)

#There is a patter found in the Residual verses Predicted plot. Hence the model form is incorrect.

#Try different polynomial orders

lm.cafe=lm(Sales~Dispensers + I(Dispensers^2),data = cafe)
summary(lm.cafe)
plot(lm.cafe)

ggplot(lm.cafe,
       aes(x=Dispensers,y=resid(lm.cafe)))+
       geom_point(color="orange",size=2)+
       labs(title="Residual plot", x="Dispensers", y="Residuals")

#Question 2

# Perform a forward selection (by hand) using the AIC criteria (you will need to use the command
# AIC(model) to get the AIC values for each model). The “smallest” model should be the just the
# intercept. The “biggest” model should be Dispensers up to the power of 4 (be sure to follow
# model hierarchy). What was the best degree for the polynomial based on AIC?

lm.mod1 <- lm(Sales~Dispensers , data = cafe)
AIC(lm.mod1)

lm.mod2 <- lm(Sales~Dispensers + I(Dispensers^2) , data = cafe)
AIC(lm.mod2)

lm.mod3 <- lm(Sales~Dispensers + I(Dispensers^2) + I(Dispensers^3), data = cafe)
AIC(lm.mod3)

lm.mod4 <- lm(Sales~Dispensers + I(Dispensers^2) + I(Dispensers^3) + I(Dispensers^4) , data = cafe)
AIC(lm.mod4)

#create histogram of residuals
ggplot(data = cafe, aes(x = lm.mod2$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#Another method
hist(lm.mod2$residuals)

#Perform a Shapiro test to validate normality of residuals.
shapiro.test(resid(lm.mod2))

#W = 0.96462, p-value = 0.7978
# Based on the p-value, we accept that the residuals are normally distributed.
