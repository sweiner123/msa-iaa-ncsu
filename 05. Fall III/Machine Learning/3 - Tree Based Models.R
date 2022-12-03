###################################
#                                 #
#         Machine Learning:       #
#         Tree-Based Models       #
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
library(AmesHousing)

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

training.df <- as.data.frame(training)

# Random Forest model
set.seed(12345)
rf.ames <- randomForest(Sale_Price ~ ., data = training.df, ntree = 500, importance = TRUE)

# Plot the change in error across different number of trees
plot(rf.ames, main = "Number of Trees Compared to MSE")

#Look at variable importance
varImpPlot(rf.ames,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf.ames)

# Tune an random forest mtry value
set.seed(12345)
tuneRF(x = training.df[,-1], y = training.df[,1], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5)

set.seed(12345)
rf.ames <- randomForest(Sale_Price ~ ., data = training.df, ntree = 500, mtry = 4, importance = TRUE)

varImpPlot(rf.ames,
           sort = TRUE,
           n.var = 14,
           main = "Order of Variables")
importance(rf.ames, type = 1)

# Interpret some of the variables using partial dependence plots
partialPlot(rf.ames, training.df, Year_Built)
partialPlot(rf.ames, training.df, Garage_Area)

# Include a random variable to determine variable selection
training.df$random <- rnorm(2051)

set.seed(12345)
rf.ames <- randomForest(Sale_Price ~ ., data = training.df, ntree = 500, mtry = 4, importance = TRUE)

varImpPlot(rf.ames,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable")
importance(rf.ames)

# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(Sale_Price ~ ., data = training)[, -1]
train_y <- training$Sale_Price

# Build XGBoost model
set.seed(12345)
xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100)

# Tuning an XGBoost nrounds parameter - 24 was lowest!
xgbcv.ames <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 10)

# Tuning through caret
tune_grid <- expand.grid(
  nrounds = 24,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(12345)
xgb.ames.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                 number = 10))

plot(xgb.ames.caret)

# Variable importance
xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5)

xgb.importance(feature_names = colnames(train_x), model = xgb.ames)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ames))

# Include a random variable to determine variable selection
training$random <- rnorm(2051)

train_x <- model.matrix(Sale_Price ~ ., data = training)[, -1]
train_y <- training$Sale_Price

set.seed(12345)
xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5, objective = "reg:linear")

xgb.importance(feature_names = colnames(train_x), model = xgb.ames)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ames))

# Interpret some of the variables using partial dependence plots
xgb.ames <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 24, eta = 0.25, max_depth = 5, objective = "reg:linear")

partial(xgb.ames, pred.var = "Year_Built", 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", 
        train = train_x, pdp.color = "red")
partial(xgb.ames, pred.var = "Garage_Area", 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", 
        train = train_x)
