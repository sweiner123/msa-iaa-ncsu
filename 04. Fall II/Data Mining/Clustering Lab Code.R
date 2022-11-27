################# BLUE 19 DATA MINING LAB ################# 
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.data)
library(ISLR2)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(ROCR)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(gridExtra)
library(NbClust)
library(dendextend)
library(class)
library(ClustOfVar)
library(MASS)
library(kableExtra)
library(partykit)
library(dbscan)
library(Hmisc)
library(caret)

########## EDA ##########
summary(teens4)

########## dummy coding for gender ###########
# impute gender var
teens4$gender <- ifelse(is.na(teens4$gender), "NA", teens4$gender)
teens4$gender <- as.factor(teens4$gender)
teens4$gradyear <- as.factor(teens4$gradyear)

# perform one-hot encoding on data frame
dummy <- dummyVars(" ~ .", data=teens4)
final_df <- data.frame(predict(dummy, newdata=teens4))

########## scale data ############
scaled <- scale(final_df)

######## check optimal # of clusters #########
set.seed(1)
fviz_nbclust(scaled, kmeans, method = "silhouette",k.max = 6)
fviz_nbclust(scaled, kmeans, method = "wss",k.max = 6)

clus2=kmeans(scaled,centers=2,nstart = 25)
clus2

clus3=kmeans(scaled,centers=3,nstart = 25)
clus3

clus4=kmeans(scaled,centers=4,nstart = 25)
clus4

clus5=kmeans(scaled,centers=5,nstart = 25)
clus5

fviz_cluster(clus3, geom = "point", data = scaled) + ggtitle("k = 3")

########### profile clusters ############
profile.kmeans=cbind(final_df,clus3$cluster)

all.k = profile.kmeans %>% group_by(clus3$cluster) %>%
  summarise(across(everything(),list(mean)))

View(all.k)
