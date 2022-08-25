library(ggplot2)
library(gmodels)

safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

# Q - 1.a)

# Unsafe: binary safety designation 
  # (1 = below average (unsafe), 0 = average or above average (safe))
# Type: type of car (Large, Medium, Small, Sport/Utility, Sports)
# Region: manufacturing region (Asia, N America)
# Weight: integer value for car weight ranging from 1 to 6
# Size: size of car corresponding to Type 
  # (1 = Small/Sports, 2 = Medium, 3 = Large or Sport/Utility)

# a. Which variables are continuous, nominal, ordinal?

# Continuous - Weight
# Nominal - Region
# Ordinal - Type, Size, Unsafe, Region

# Q - 1.b)
# 
# Examine the association between Region and Unsafe.

table(safety$Region, safety$Unsafe)

prop.table(table(safety$Region, safety$Unsafe))

CrossTable(safety$Region, safety$Unsafe)

#   a. What percentage of cars manufactured in Asia was classified as unsafe? - 0.429
#   b. What percentage of cars classified as safe was manufactured in North America? - 0.697

#   c. What is the appropriate test to use? - Chi Square Test
#     i. State the null and alternative hypothesis for the test.
#        - Null: There is no association between region and Unsafe.
#        - Alt: There is some relationship between region and Unsafe.

#     ii. At an alpha of 0.05, what is your decision?

chisq.test(table(safety$Region, safety$Unsafe))

# p-value = 0.1031

# This implies that we have statistical evidence that there is no relationship 
# between Region and Unsafe.

fisher.test(table(safety$Region, safety$Unsafe))

# d. Regardless of significance, interpret the odds ratio in the context of the
# problem. 

library(DescTools)
OddsRatio(table(safety$Region, safety$Unsafe))

# 0.4347826
# Row 1 divided by Row 2 in terms of Column 1. That's how 'R' looks at odd ratios for interpretation.
# Vehicles in Asia have a 0.434 times the odds of being Safe as compared to Vehicles in N America. 

# Q - 1c)

# c. Examine the association between Size and Unsafe.

# a. What is the appropriate test to use for association?
#      - ORDINAL vs ORDINAL TEST - MH Test

#   i. At an alpha of 0.05, what is your decision?

library(vcdExtra)
x = CMHtest(table(safety$Size, safety$Unsafe))$table[1,]

#   b. How strong is the association between these variables?

assocstats(table(safety$Size, safety$Unsafe))

# Cramer's V is Not very interpretable 

cor.test(x = as.numeric(ordered(safety$Size)), 
         y = as.numeric(ordered(safety$Unsafe)), 
         method = "spearman")

# rho 
# -0.5424769

# A negative directional relationship


