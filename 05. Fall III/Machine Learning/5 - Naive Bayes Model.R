###################################
#                                 #
#         Machine Learning:       #
#        Naive Bayes Models       #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(e1071)
library(ggplot2)
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

# Naive Bayes model
set.seed(12345)
nb.ames <- naiveBayes(Sale_Price ~ ., data = training, laplace = 0, usekernel = TRUE)

# Optimize laplace and kernel - CARET ONLY ABLE TO TUNE CLASSIFICATION PROBLEMS FOR NAIVE BAYES
tune_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = c(0, 0.5, 1)
)

set.seed(12345)
nb.ames.caret <- train(... ~ ., data = training,
                       method = "nb", 
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))