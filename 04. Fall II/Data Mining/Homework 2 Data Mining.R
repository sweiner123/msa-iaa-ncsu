# Homework 2 - Data Mining

# Libs to load
library(rpart)
#install.packages("partykit")
library(partykit)
library(dplyr)
library(rpart.plot)
library(ggplot2)
library("InformationValue")
#install.packages("Metrics")
library(Metrics)
#install.packages("mlr")
library(mlr)

# Load csv
churn <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/TelcoChurn.csv")

length(unique(churn$customerID))
#7,043 customers

#make this example reproducible
set.seed(1)

# Split to training and test
train <- sample_frac(churn, 0.8)
test <- anti_join(churn, train, by ='customerID')

dim(train)
dim(test)

prop.table(table(train$Churn))
prop.table(table(test$Churn))

train_new <- churn[1:floor(0.8*7043),]
test_new <- churn[(floor(0.8*7043)+1):7043,]

dim(train_new)
dim(test_new)


prop.table(table(train_new$Churn))
prop.table(table(test_new$Churn))

## DTree Model 1

dtree_model <- rpart(Churn ~ . -customerID,data = train, method = 'class', parms = list(split = 'gini'))
summary(dtree_model)

rpart.plot(dtree_model)

varimp_data <- data.frame(dtree_model$variable.importance)
varimp_data$names <- as.character(rownames(varimp_data))

ggplot(data = varimp_data, aes(x = reorder(names, dtree_model.variable.importance), y = dtree_model.variable.importance)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Variable Name", y = "Variable Importance") +
  geom_text(aes(label = scales::comma(dtree_model.variable.importance)), hjust = 0, color = "black")



#Make Predictions

predicted <-predict(dtree_model, test, type = 'class')

train_scores = predict(dtree_model,type='class')
test_scores = predict(dtree_model, test, type='class')

##Training misclassification rate:
sum(train_scores!=train$Churn)/nrow(train)

#0.2028754

### Test data:
sum(test_scores!=test$Churn)/nrow(test)

#0.2150461

# Dtree 2 Model (with the new sequential split)

table(churn$Churn)

table(train_new$Churn)

prop.table(table(train_new$Churn))

#No  Yes 
#1028  381 

dtree_model_new <- rpart(Churn ~ . -customerID,data = train_new, method = 'class', parms = list(split = 'gini'))
summary(dtree_model_new)

rpart.plot(dtree_model_new)


#best <- dtree_model_new$cptable[which.min(dtree_model_new$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
#pruned_tree <- prune(dtree_model_new, cp=best)

#plot the pruned tree
#prp(pruned_tree)

#Make Predictions

predicted <-predict(dtree_model_new, test_new, type = 'class')

train_scores = predict(dtree_model_new,type='class')
test_scores = predict(dtree_model_new, test_new, type='class')

##Training misclassification rate:
sum(train_scores!=train_new$Churn)/nrow(train_new)

#0.2037629

### Test data:
sum(test_scores!=test_new$Churn)/nrow(test_new)

#0.2008517

#plot decision tree using custom arguments
prp(dtree_model_new,
    faclen=0, #use full names for factor labels
    extra=101, #display number of observations for each terminal node
    roundint=F, #don't round to integers in output
    digits=5,
    type=4)

cols <- ifelse(dtree_model_new$frame$yval == 1, "darkred", "green4")

prp(dtree_model_new,
    extra = 109,           # display prob of survival and percent of obs
    nn = TRUE,             # display the node numbers
    fallen.leaves = TRUE,  # put the leaves on the bottom of the page
    shadow.col = "gray",   # shadows under the leaves
    branch.lty = 2,        # draw branches using dotted lines
    branch = .5,           # change angle of branch lines
    faclen = 0,            # faclen = 0 to print full factor names
    trace = 1,             # print the auto calculated cex, xlim, ylim
    split.cex = 1.2,       # make the split text larger than the node text
    split.prefix = "is ",  # put "is " before split text
    split.suffix = "?",    # put "?" after split text
    col = cols, border.col = cols,   # green if survived
    split.box.col = "lightgray",   # lightgray split boxes (default is white)
    split.border.col = "darkgray", # darkgray border on split boxes
    split.round = .5,
    type=4) 


### Pruned Tree

predicted <-predict(pruned_tree, test_new, type = 'class')

train_scores = predict(pruned_tree,type='class')
test_scores = predict(pruned_tree, test_new, type='class')

##Training misclassification rate:
sum(train_scores!=train_new$Churn)/nrow(train_new)

### Test data:
sum(test_scores!=test_new$Churn)/nrow(test_new)


#How many NO's and YES's in the test data?
table(test_new$Churn)

#No  Yes 
#1028  381 

#How many NO's and YES's in the predicted data by the model?
table(as.data.frame(predicted))

#No  Yes 
#1141  268

## to get the probabilities of each record on test data
probilities_ <- predict(dtree_model_new, test_new, type = "prob")

#Plot the confusion matrix on Test data
confusionMatrix(test_new$Churn, probilities_[,2], threshold = 0.5)

#Actual on columns and predicted on rows

#   No Yes
#0 943 198
#1  85 183

# Getting Accuracy (install Metrics library)
accuracy(test_new$Churn, predicted)
#0.7991483

### Decision Tree Model 3 with Information

# Dtree 3 Model (with the new sequential split)

dtree_model_3 <- rpart(Churn ~ . -customerID,data = train_new, method = 'class', parms = list(split = 'information'))
summary(dtree_model_3)

rpart.plot(dtree_model_3)

#Make Predictions

predicted <-predict(dtree_model_3, test_new, type = 'class')

train_scores = predict(dtree_model_3,type='class')
test_scores = predict(dtree_model_3, test_new, type='class')

##Training misclassification rate:
sum(train_scores!=train_new$Churn)/nrow(train_new)



### Test data:
sum(test_scores!=test_new$Churn)/nrow(test_new)


#######


# Dtree 3 Model (Let's tweak some hyperparameters)
dtree_model_opt <- rpart(Churn ~ . -customerID,data = train_new, method = 'class', parms = list(split = 'gini'),control = c(maxdepth = 5, cp=0.01))

rpart.plot(dtree_model_opt)

train_scores = predict(dtree_model_opt,type='class')
test_scores = predict(dtree_model_opt, test_new, type='class')

##Training misclassification rate:
sum(train_scores!=train_new$Churn)/nrow(train_new)

#0.1959531

### Test data:
sum(test_scores!=test_new$Churn)/nrow(test_new)

#0.2093683

# This model is not better either





