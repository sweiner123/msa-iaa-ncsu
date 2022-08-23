library(tidyverse)
library(AmesHousing)
library(ggplot2)

ames <- make_ordinal_ames() 
str(ames)


set.seed(123)
ames <- ames %>% mutate(id = row_number())

train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

# Scatter Plots

ggplot(data = train) +
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price/1000)) +
  labs(y = "Sales Price (Thousands $)", x = "Greater Living Area
(Sqft)")

#Grouped Scatter Plots

ggplot(data = train, aes(y = Sale_Price/1000,
                         x = Exter_Qual,
                         fill = Exter_Qual)) +
  geom_boxplot() +
  labs(y = "Sales Price (Thousands $)",
       x = "Exterior Quality Category") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "red",
               fill = "red") +
  scale_fill_brewer(palette="Blues") +
  theme_classic() + coord_flip()

# Overlaid Histograms in R

ggplot(ames,aes(x = Sale_Price/1000, fill = Exter_Qual)) +
  geom_density(alpha = 0.2, position = "identity") +
  labs(x = "Sales Price (Thousands $)")

#ANOVA in R

ames_lm <- lm(Sale_Price ~ Exter_Qual, data = train)

anova(ames_lm)
summary(ames_lm)

train$pred_anova <- predict(ames_lm, data = train)

train$resid_anova <- resid(ames_lm, data = train)

ames_aov <- aov(Sale_Price ~ Exter_Qual, data = train)
tukey.ames <- TukeyHSD(ames_aov)
print(tukey.ames)

#### LAB 3

# Q - 1
garlic <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/garlic.csv")

garlic$Fertilizer <- as.factor(garlic$Fertilizer)

garlic_lm <- lm(BulbWt ~ Fertilizer, data = garlic)

anova(garlic_lm)

par(mfrow=c(2,2))
plot(garlic_lm)
par(mfrow=c(1,1))

#Q - 2 a)

ggplot(data = garlic, aes(y = BulbWt,
                         x = Fertilizer,
                         fill = Fertilizer)) +
  geom_boxplot() +
  labs(y = "Bulb Weight",
       x = "Fertiliz Type") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "red",
               fill = "red") +
  scale_fill_brewer(palette="Blues") +
  theme_classic() + coord_flip()

## Q-Q Plots to check normality

ggplot(data = garlic, aes(sample = BulbWt, color = Fertilizer)) +
  stat_qq() +
  stat_qq_line()

install.packages('car')
install.packages('stats')

library(car)
library(stats)

leveneTest(BulbWt ~ Fertilizer, data = garlic)

fligner.test(BulbWt ~ Fertilizer, data = garlic)

kruskal.test(BulbWt ~ Fertilizer, data = garlic)

ames_aov <- aov(BulbWt ~ Fertilizer, data = garlic)
tukey.ames <- TukeyHSD(ames_aov)
print(tukey.ames)


## Q - 3 - Bottle Question

bottle <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bottle.csv")

bottle$Line <- as.factor(bottle$Line)

bottle_lm <- lm(Units ~ Line, data = bottle)

anova(bottle_lm)

leveneTest(Units ~ Line, data = bottle)

fligner.test(Units ~ Line, data = bottle)

kruskal.test(Units ~ Line, data = bottle)

bottle_aov <- aov(Units ~ Line, data = bottle)
tukey.ames <- TukeyHSD(bottle_aov)
print(tukey.ames)


#Q - 3








