###################################
#                                 #
#         Machine Learning:       #
#       Neural Network Models     #
#                                 #
#          Dr Aric LaBarr         #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(nnet)
library(NeuralNetTools)
library(ggplot2)
library(reshape2)
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

# Standardizing Continuous Variables
training <- training %>%
  mutate(s_SalePrice = scale(Sale_Price),
         s_Bedroom_AbvGr = scale(Bedroom_AbvGr),
         s_Year_Built = scale(Year_Built),
         s_Mo_Sold = scale(Mo_Sold),
         s_Lot_Area = scale(Lot_Area),
         s_First_Flr_SF = scale(First_Flr_SF),
         s_Second_Flr_SF = scale(Second_Flr_SF),
         s_Garage_Area = scale(Garage_Area),
         s_Gr_Liv_Area = scale(Gr_Liv_Area),
         s_TotRms_AbvGrd = scale(TotRms_AbvGrd))

training$Full_Bath <- as.factor(training$Full_Bath)
training$Half_Bath <- as.factor(training$Half_Bath)
training$Fireplaces <- as.factor(training$Fireplaces)

# Neural Network model
set.seed(12345)
nn.ames <- nnet(Sale_Price ~ 
                  s_Bedroom_AbvGr + 
                  s_Year_Built + 
                  s_Mo_Sold + 
                  s_Lot_Area + 
                  s_First_Flr_SF + 
                  s_Second_Flr_SF + 
                  s_Garage_Area + 
                  s_Gr_Liv_Area +
                  s_TotRms_AbvGrd + 
                  Street + 
                  Central_Air +
                  Full_Bath +
                  Half_Bath +
                  Fireplaces
                , data = training, size = 5, linout = TRUE)

# Plot the network
plotnet(nn.ames)

# Optimize Number of Hidden Nodes and Regularization (decay option)
tune_grid <- expand.grid(
  .size = c(3, 4, 5, 6, 7),
  .decay = c(0, 0.5, 1)
)

set.seed(12345)
nn.ames.caret <- train(Sale_Price ~ 
                         s_Bedroom_AbvGr + 
                         s_Year_Built + 
                         s_Mo_Sold + 
                         s_Lot_Area + 
                         s_First_Flr_SF + 
                         s_Second_Flr_SF + 
                         s_Garage_Area + 
                         s_Gr_Liv_Area +
                         s_TotRms_AbvGrd + 
                         Street + 
                         Central_Air +
                         Full_Bath +
                         Half_Bath +
                         Fireplaces
                       , data = training,
                       method = "nnet", # Neural network using the nnet package
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10),
                       trace = FALSE, linout = TRUE)

nn.ames.caret$bestTune

set.seed(12345)
nn.ames <- nnet(Sale_Price ~ 
                  s_Bedroom_AbvGr + 
                  s_Year_Built + 
                  s_Mo_Sold + 
                  s_Lot_Area + 
                  s_First_Flr_SF + 
                  s_Second_Flr_SF + 
                  s_Garage_Area + 
                  s_Gr_Liv_Area +
                  s_TotRms_AbvGrd + 
                  Street + 
                  Central_Air +
                  Full_Bath +
                  Half_Bath +
                  Fireplaces
                , data = training, size = 6, decay = 1, linout = TRUE)

plotnet(nn.ames)

# Hinton Diagram
nn_weights <- matrix(data = nn.ames$wts[1:132], ncol = 6, nrow = 22)
rownames(nn_weights) <- c("bias", nn.ames$coefnames)
colnames(nn_weights) <- c("h1", "h2", "h3", "h4", "h5", "h6")

ggplot(melt(nn_weights), aes(x=Var1, y=Var2, size=abs(value), color=as.factor(sign(value)))) +
  geom_point(shape = 15) +
  scale_size_area(max_size = 40) +
  labs(x = "", y = "", title = "Hinton Diagram of NN Weights") +
  theme_bw()