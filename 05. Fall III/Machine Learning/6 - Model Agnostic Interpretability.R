###################################
#                                 #
#         Machine Learning:       #
# Model Agnostic Interpretability #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(ALEPlot)
library(lime)
library(iml)
library(AmesHousing)

# Load the Ames, Iowa dataset and create a train and test split. 
ames <- make_ordinal_ames()

ames <- ames %>% mutate(id = dplyr::row_number())

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

training.df <- as.data.frame(training)

# Random Forest model
set.seed(12345)
rf.ames <- randomForest(Sale_Price ~ ., data = training.df, ntree = 250, importance = TRUE)

#Look at variable importance
varImpPlot(rf.ames,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance", type = 1)
importance(rf.ames)

training.df$pred_rf <- predict(rf.ames)

# Permutation Importance for Linear Regression
lm.ames <- lm(Sale_Price ~ ., data = training)

summary(lm.ames)

linear_pred <- Predictor$new(lm.ames, data = training[,-1], 
                             y = training$Sale_Price, type = "response")
plot(FeatureImp$new(linear_pred, loss = "mse"))

# ICE for Random Forest
set.seed(12345)
forest_pred <- Predictor$new(rf.ames, data = training[,-1], 
                             y = training$Sale_Price, type = "response")

ice_plot <- FeatureEffects$new(forest_pred, method = "ice")
ice_plot$plot(c("Garage_Area"))
ice_plot$plot()

# PDP for Random Forest
pd_plot <- FeatureEffects$new(forest_pred, method = "pdp")
pd_plot$plot(c("Garage_Area"))
pd_plot$plot()

# PDP on ICE for Random Forest
pdice_plot <- FeatureEffects$new(forest_pred, method = "pdp+ice")
pdice_plot$plot(c("Garage_Area"))
pdice_plot$plot()

pdice_plot$plot(c("Mo_Sold"))

# ALE for Random Forest
ale_plot <- FeatureEffects$new(forest_pred, method = "ale")
ale_plot$plot(c("Garage_Area"))
ale_plot$plot()

# LIME for Random Forest
point <- 1328
lime.explain <- LocalModel$new(forest_pred, x.interest = training[point,-1], k = 5)
plot(lime.explain) + theme_bw()

point <- 1000
lime.explain <- LocalModel$new(forest_pred, x.interest = training[point,-1], k = 5)
plot(lime.explain)

# Shapley Values for Random Forest
point <- 1328
shap <- Shapley$new(forest_pred, x.interest = training[point,-1])
shap$plot()

point <- 1000
shap <- Shapley$new(forest_pred, x.interest = training[point,-1])
shap$plot()