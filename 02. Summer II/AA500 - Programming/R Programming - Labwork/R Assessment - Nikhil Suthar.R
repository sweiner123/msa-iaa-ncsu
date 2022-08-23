
# R Programming Assessment

install.packages("lubricate")
library(tidyverse)
library(lubridate)

#Set the working directory

setwd("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\5. Week 5 - July 18 to July 22\\R Assessment\\Programming Assessment Files-20220719\\Data\\Data\\R data")

#Read the data
traffic <- read.csv('Traffic_stops.csv', stringsAsFactors = FALSE)

#Calculate Birth Year
traffic <- traffic %>%
  rowwise() %>%
  mutate(Birth_Year = as.numeric(str_split(driver_birthdate,"/")[[1]][3]))

#Split into Cohorts
traffic <- traffic %>%
  rowwise() %>%
  mutate(Cohort = ifelse(Birth_Year >= 2000,"after-2000","pre-2000"))

# Most frequent birth years
traffic %>% 
  group_by(Birth_Year)%>%
  count() %>%
  arrange(desc(n))

#How many pre-2000 observations?
traffic_pre <- traffic %>% filter(Cohort == "pre-2000")

#211061

# What is the mean age for Careless driving?

summary(traffic$driver_age)

# There are 109 NA's values in the driver_age column.

traffic %>% 
  group_by(violation) %>%
  summarize(Mean_Age=mean(driver_age, na.rm = TRUE))

#35.4

# What is the mean age for males with a Seat belt violation?

traffic %>% 
  group_by(driver_gender,violation) %>%
  summarize(Mean_Age=mean(driver_age, na.rm = TRUE)) %>%
  filter(driver_gender == "male",violation == "Seat belt")

#38.3

# New data frame with female drivers of age > 60 that were stopped on a Sunday

sub_frame <- traffic %>% filter(driver_gender == "female", driver_age > 60)

sub_frame <- sub_frame %>%
  rowwise() %>%
  mutate(weekday_of_stop = wday(mdy(stop_date)))

sub_frame <- sub_frame %>% select(violation,weekday_of_stop)

sub_frame <- sub_frame %>% filter(weekday_of_stop==1)

# How many observations in the data? 

# 407 observations

# What is the most frequent driving violation in this new dataframe?

sub_frame %>%
  group_by(violation) %>%
  count() %>%
  arrange(desc(n))
  
# Speeding

