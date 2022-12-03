###################################
#                                 #
#         Machine Learning:       #
#  Validation and Regularization  #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
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

# Cross-validation in stepwise regression - don't forget to set your seed!
set.seed(9876)

step.model <- train(Sale_Price ~ ., data = training,
                    method = "leapBackward", # Uses the leap package for backward selection
                    tuneGrid = data.frame(nvmax = 1:14), # Maximum number of variables to have in model - our tuning parameter
                    trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                             number = 10))

step.model$results # Shows results from best model with each number of variables
step.model$bestTune # Shows how many variables were optimal
summary(step.model$finalModel) # Shows the variables from the optimal model

# Build model using the variables selected above (classic approach)
final.model1 <- glm(Sale_Price ~ First_Flr_SF + 
                      Second_Flr_SF + 
                      Year_Built + 
                      Garage_Area + 
                      Bedroom_AbvGr +
                      Fireplaces,
                    data = training)

summary(final.model1)

# Build model using the NUMBER of variables selected above (modern approach)
empty.model <- glm(Sale_Price ~ 1, data = training)
full.model <- glm(Sale_Price ~ ., data = training)

final.model2 <- step(empty.model, scope = list(lower = formula(empty.model),
                                               upper = formula(full.model)),
                     direction = "both", steps = 6)

summary(final.model2) # Notice how there are DIFFERENT variables here. That is fine. We tuned the parameter of number of variables.

# Elastic Net model tuned for both lambda as well as alpha
set.seed(5)

en.model <- train(Sale_Price ~ ., data = training,
                  method = "glmnet", # Uses the leap package for backward selection
                  tuneGrid = expand.grid(.alpha = seq(0,1, by = 0.05),
                                         .lambda = seq(100,60000, by = 1000)), # Define grid search for both tuning parameters - alpha and lambda
                  trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                           number = 10))
en.model

# Build elastic net model with optimal alpha and view lambda values across cross-validation
train_x <- model.matrix(Sale_Price ~ ., data = training)[, -1]
train_y <- training$Sale_Price

ames_en <- glmnet(x = train_x,  y = train_y,  alpha = 0.5) # Got the alpha = 0.5 from above grid search

plot(ames_en, xvar = "lambda")

# Can also use CV inside of glmnet function at specified alpha level
set.seed(5)
ames_en_cv <- cv.glmnet(x = train_x,  y = train_y,  alpha = 0.5) #Got alpha = 0.1 from above grid search

plot(ames_en_cv) # At an alpha = 0.1, we can search across different levels of lambda

ames_en_cv$lambda.min # Around the same lambda that we had from the grid search, but this is more granular
ames_en_cv$lambda.1se # Can always go up 1 standard error to get more variable reduction
