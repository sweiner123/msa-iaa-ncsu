library(ggplot2)
library(tidyverse)
library(gmodels)
library(vcdExtra)
library(DescTools)

# Read in the safety data
safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

# Examine relationship between region and unsafe
table(safety$Region, safety$Unsafe)

ggplot(data = safety) +
  geom_bar(mapping = aes(x = Unsafe, fill = Region))

CrossTable(safety$Region, safety$Unsafe)

CMHtest(table(safety$Region, safety$Unsafe))$table[1,]

table(safety$Region, safety$Unsafe)
OddsRatio(table(safety$Region, safety$Unsafe))

1/OddsRatio(table(safety$Region, safety$Unsafe))

# Examine relationship between size and unsafe
prop.table(table(safety$Size, safety$Unsafe))

ggplot(data = safety) +
  geom_bar(mapping = aes(x = Unsafe, fill = factor(Size)))

CrossTable(safety$Size, safety$Unsafe)

CMHtest(table(safety$Size, safety$Unsafe))$table[1,]

cor.test(x = as.numeric(ordered(safety$Size)), 
         y = as.numeric(ordered(safety$Unsafe)), 
         method = "spearman")
