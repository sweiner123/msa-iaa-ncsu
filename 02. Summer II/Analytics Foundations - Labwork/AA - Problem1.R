library(AmesHousing)
library(ggplot2)

ames <- make_ordinal_ames() 
str(ames)
str(ames,give.attr=F)

ggplot(data = ames) +
  geom_histogram(mapping = aes(x = Sale_Price/1000)) +
  labs(x = "Sales Price")

ggplot(data = ames,aes(x = Sale_Price/1000)) +
  geom_histogram(mapping = ) +
  labs(x = "Sales Price")


ggplot(data = ames, aes(sample = Sale_Price/1000)) +
  stat_qq() +
  stat_qq_line()

ggplot(data = ames, aes(y = Sale_Price/1000, x = Central_Air, fill = Central_Air)) + 
  geom_boxplot() + 
  labs(y = "Sales Price (Thousands $)", x = "Central Air") +
  scale_fill_brewer(palette="Accent") + theme_classic() + coord_flip()


library(unc)


unc <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/unc.csv")

str(unc,give.attr=F)

mean(unc$salary)


## Lab 1 - NORM TEMP DATA

install.packages('UsingR')
library(UsingR)
data(normtemp)

min(normtemp$temperature)
max(normtemp$temperature)
mean(normtemp$temperature)
sd(normtemp$temperature)

ggplot(data = normtemp) +
  geom_histogram(mapping = aes(x = temperature)) +
  labs(x = "Temperature")

ggplot(data = normtemp, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line()

ggplot(data = normtemp, aes(y = temperature)) + 
  geom_boxplot() + 
  labs(y = "Temperature") + 
  geom_hline(yintercept=98.6) +
  geom_vline(xintercept=98.6)


## Lab 1 - AMES HOUSING DATA

#Sale_Price, Log(Sale_Price), and Gr_Liv_Area.

ggplot(data = ames) +
  geom_histogram(mapping = aes(x = Sale_Price)) +
  labs(x = "Sales Price")

logSale = log(ames$Sale_Price)

ggplot(data = ames) +
  geom_histogram(mapping = aes(x = logSale)) +
  labs(x = "Log of Sales Price")

ggplot(data = ames) +
  geom_histogram(mapping = aes(x = Gr_Liv_Area)) +
  geom_density(aes(x=Gr_Liv_Area),alpha = 0.2) +
  labs(x = "Gr Liv Area")

ggplot(data = ames,aes(x = Sale_Price)) +
  geom_histogram(aes(y=..density..),alpha = 0.5 ) +
  geom_density(alpha = 0.2) +
  labs(x = "Sale_Price")







