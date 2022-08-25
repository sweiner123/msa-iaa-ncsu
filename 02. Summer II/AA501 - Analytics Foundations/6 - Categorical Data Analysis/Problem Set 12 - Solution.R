library(ggplot2)
library(tidyverse)
library(gmodels)
library(vcdExtra)
library(DescTools)


# Read in the bike data
bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')

# Split into training and test datasets
set.seed(123)

bike <- bike %>% mutate(id = row_number())

train <- bike %>% sample_frac(0.7)

test <- anti_join(bike, train, by = 'id')

# Create the casual_high variable
train$casual_high <- train$casual >= train$registered

table(train$casual_high)

# Chi-square test and cross-tabulation table between casual_high and season
CrossTable(train$season, train$casual_high)

chisq.test(table(train$season, train$casual_high))
CMHtest(table(train$season, train$casual_high))$table[1,]

# Chi-square test and cross-tabulation table between casual_high and holiday
CrossTable(train$holiday, train$casual_high)

CMHtest(table(train$holiday, train$casual_high))$table[1,]

OddsRatio(table(train$holiday, train$casual_high))
