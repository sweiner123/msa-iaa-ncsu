library(AmesHousing)
library(ggplot2)

ames <- make_ordinal_ames() 
str(ames)


t.test(ames$Sale_Price, mu = 178000)

install.packages('UsingR')
library(UsingR)
data(normtemp)
library(ggplot2)

#Q - 1 a)

mean(normtemp$temperature)

t.test(normtemp$temperature, mu = 98.6)

var.test(data = normtemp)

#Q - 1 b)



#Only females Q-1 c)

normtemp_males <- normtemp[normtemp$gender == 1,]
normtemp_females <- normtemp[normtemp$gender == 2,]

t.test(normtemp_females$temperature, mu = 98.6)

## verify normality

ggplot(data = normtemp_males, aes(sample = temperature, color = gender)) +
  stat_qq() +
  stat_qq_line()

ggplot(data = normtemp_females, aes(sample = temperature, color = gender)) +
  stat_qq() +
  stat_qq_line()

## verify equality of variances
var.test(temperature ~ gender, data = normtemp)
#p-value = 0.6211 hence we failt to reject the null hypothesis


t.test(temperature ~ gender, data = normtemp, var.equal = FALSE)

#Q - 2

data(AirPassengers)
install.packages('tseries')
install.packages('forecast')
library(tseries)
library(forecast)
cycle(AirPassengers)

install.packages("tidyverse")
library(tidyverse)


air1 = data.frame(AirPassengers)
air2 = air1 %>% mutate(summer=ifelse(cycle(AirPassengers) %in%
                                       6:8,1,0))

## Pipe operators, mutate will filter

t.test(air1 ~ air2, data = normtemp, var.equal = FALSE)



