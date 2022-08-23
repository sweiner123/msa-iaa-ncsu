library(ggplot2)
library(tidyverse)

#Read the data

drugdose <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/drug.csv')

drugdose$DrugDose <- as.factor(drugdose$DrugDose)
drugdose$Disease <- as.factor(drugdose$Disease)

ggplot(data = drugdose, aes(x = DrugDose, y = BloodP, fill = Disease)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Change in Blood Pressure", x = "DrugDose Category") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

drug_aov2 <- aov(BloodP ~ DrugDose*Disease, data = drugdose)

summary(drug_aov2)

CA_aov <- drugdose %>%
  group_by(Disease) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(BloodP ~ DrugDose, data = .x))))

print(CA_aov$aov)


#Technician Data

disks = read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/disks.csv')

disks$Brand <- as.factor(disks$Brand)

disks_aov2 <- aov(Time ~ Technician*Brand, data = disks)

summary(disks_aov2)

disks_sliced <- disks %>%
  group_by(Technician) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(Time ~ Brand, data = .x))))

print(disks_sliced$aov)


# Bike dataset

bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')

bike$season <- as.factor(bike$season)
bike$workingday <- as.factor(bike$workingday)

bike_aov_int <- aov(cnt ~ season*workingday, data = bike)
summary(bike_aov_int)

tukey.bike_int <- TukeyHSD(bike_aov_int)
plot(tukey.bike_int, las = 1)


#Seperate Casual & Registered Users

CA_aov <- bike %>%
  group_by(Central_Air) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(Sale_Price ~ Heating_QC, data = .x))))
print(CA_aov$aov)





