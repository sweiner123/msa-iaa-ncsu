###################################
#                                 #
#         Machine Learning:       #
#   Generalized Additive Models   #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(ggplot2)
library(earth)
library(mgcv)
library(AmesHousing)

# Load cement dataset for piece-wise linear regression example
#setwd("C:\\Users\\adlabarr\\Google Drive\\IAA\\Courses\\IAA\\Machine Learning\\Data")
cement <- read.csv("cement.csv", header = TRUE)

# Plots with ratio by strength (scatterplot, scatterplot with linear regression, scatterplot with piece-wise linear regression)
ggplot(cement, aes(x = RATIO, y = STRENGTH)) +
  geom_point() +
  ylim(0,6)

ggplot(cement, aes(x = RATIO, y = STRENGTH)) +
  geom_point() +
  stat_smooth(method = 'lm') +
  ylim(0,6)

qplot(RATIO, STRENGTH, group = X2, geom = c('point', 'smooth'), method = 'lm', data = cement, ylim = c(0,6))

# Piece-wise linear regression for cement dataset
cement.lm <- lm(STRENGTH ~ RATIO + X2STAR, data = cement)

summary(cement.lm)

ggplot(cement, aes(x = RATIO, y = STRENGTH)) +
  geom_point() +
  geom_line(data = cement, aes(x = RATIO, y = cement.lm$fitted.values)) +
  ylim(0,6)

# Piece-wise linear regression for cement dataset with discontinuous point
cement.lm <- lm(STRENGTH ~ RATIO + X2STAR + X2, data = cement)

summary(cement.lm)

qplot(RATIO, STRENGTH, group = X2, geom = c('point', 'smooth'), method = 'lm', data = cement, ylim = c(0,6))

# Load the Ames, Iowa dataset and create a train and test split. 
ames <- make_ordinal_ames()

ames <- ames %>% mutate(id = row_number())

set.seed(4321)

training <- ames %>% sample_frac(0.7)
testing <- anti_join(ames, training, by = 'id')

# Reduce down the number of variables only for ease of computation.
training <- training %>% 
  select(Sale_Price,
         Bedroom_AbvGr,
         Year_Built,
         Mo_Sold,
         Lot_Area,
         Street,
         Central_Air,
         First_Flr_SF,
         Second_Flr_SF,
         Full_Bath,
         Half_Bath,
         Fireplaces,
         Garage_Area,
         Gr_Liv_Area, 
         TotRms_AbvGrd)

# EARTH (MARS) implementation
ggplot(training, aes(x = Garage_Area, y = Sale_Price)) +
  geom_point()

mars1 <- earth(Sale_Price ~ Garage_Area, data = training)
summary(mars1)

# Look at predicted values for Garage_Area to see the piecewise linear regression
ggplot(training, aes(x = Garage_Area, y = Sale_Price)) +
  geom_point() +
  geom_line(data = training, aes(x = Garage_Area, y = mars1$fitted.values), color = "blue")

# EARTH on all variables
mars2 <- earth(Sale_Price ~ ., data = training)
summary(mars2)

# Variable importance metric
evimp(mars2)

# GAM's using splines
gam1 <- mgcv::gam(Sale_Price ~ s(Garage_Area), data = training)
summary(gam1)

# Plot spline
plot(gam1)

# GAM's on all variables
gam2 <- mgcv::gam(Sale_Price ~ s(Bedroom_AbvGr, k = 5) + # The k = option sets max for EDF. Might need to use with variables with only a few unique values (< 15)
                    s(Year_Built) +
                    s(Mo_Sold) +
                    s(Lot_Area) +
                    s(First_Flr_SF) + 
                    s(Second_Flr_SF) +
                    s(Garage_Area) +
                    s(Gr_Liv_Area) +
                    s(TotRms_AbvGrd) +
                    Street +
                    Central_Air +
                    factor(Fireplaces) + 
                    factor(Full_Bath) +
                    factor(Half_Bath)
                  , method = 'REML', data = training)
summary(gam2)

# Select option in GAM to shrink spline variables by penalizing the EDF
sel.gam2 <- mgcv::gam(Sale_Price ~ s(Bedroom_AbvGr, k = 5) + 
                        s(Year_Built) +
                        s(Mo_Sold) +
                        s(Lot_Area) +
                        s(First_Flr_SF) + 
                        s(Second_Flr_SF) +
                        s(Garage_Area) +
                        s(Gr_Liv_Area) +
                        s(TotRms_AbvGrd) +
                        Street +
                        Central_Air +
                        factor(Fireplaces) + 
                        factor(Full_Bath) +
                        factor(Half_Bath)
                      , method = 'REML', select = TRUE, data = training)
summary(sel.gam2)

# Remaining variables after selection
sel.gam2 <- mgcv::gam(Sale_Price ~ s(Bedroom_AbvGr, k = 5) + 
                        s(Year_Built) +
                        s(Lot_Area) +
                        s(First_Flr_SF) + 
                        s(Second_Flr_SF) +
                        s(Garage_Area) +
                        s(Gr_Liv_Area) +
                        s(TotRms_AbvGrd) +
                        Central_Air +
                        factor(Fireplaces) + 
                        factor(Half_Bath)
                      , method = 'REML', data = training)
summary(sel.gam2)

# View splines - all plots have same Y axis which makes difficult to see
plot(sel.gam2)