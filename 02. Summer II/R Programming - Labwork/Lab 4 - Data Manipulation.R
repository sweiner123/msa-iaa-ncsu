# Q - 1

install.packages(c("data.table","dplyr","stringr"))

library(data.table)
library(dplyr)
library(stringr)

setwd("C:\\Users\\nikhi\\OneDrive\\Desktop\\MSA Folder\\2. Week 2 - June 27 to July 1\\R Lessons")
getwd()

df_auto <- read.csv("Auto_MPG.csv", stringsAsFactors = FALSE)
ncol(df_auto)
# class(df_auto)
# View(df_auto)
# dim(df_auto)
# nrow(df_auto)
# summary(df_auto)

# Q - 2

df_auto$Horsepower <- as.numeric(df_auto$Horsepower)
summary(df_auto)
df_auto$Horsepower
summary(df_auto$Horsepower)

# Q - 3

?str_split

df_auto <- df_auto %>% 
  rowwise() %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>% 
  mutate(Car_Model=paste(str_split(Car_Name," ")[[1]][-1], collapse=" ")) #everything but the first 

df_auto %>% 
  group_by(Car_Make) %>%
  summarise(no_of_obs=n()) %>%
  filter(Car_Make=='ford')
View(df_auto)

# Q - 4
df_auto %>% 
  group_by(Model_Year) %>%
  summarise(no_of_obs=n()) %>%
  filter(Model_Year==73)

# Q - 5

mutate(df_auto, Weight_per_Horsepower=Weight/Horsepower)

df_auto %>% #note the pipe operator 
  mutate(Weight_per_Horsepower=Weight/Horsepower) %>%
  filter(Car_Make=='ford') %>%
  arrange(desc(Weight_per_Horsepower)) %>%
  select(Car_Make,Car_Model,Weight_per_Horsepower)
  
# Q - 6
temp_df <- df_auto %>% #note the pipe operator 
  mutate(Car_Make = replace(Car_Make, str_detect(Car_Make, "chev"), "chevrolet")) %>%
  group_by(Car_Make) %>%
  summarise(no_of_obs=n()) %>%
  filter(Car_Make=="chevrolet")

temp_df <- df_auto %>% #note the pipe operator 
  mutate(Car_Make = replace(Car_Make, str_detect(Car_Make, "chev"), "chevrolet"))


# Q - 7
check_df <- temp_df %>%
  group_by(Car_Make) %>%
  summarise(Average_MPG=mean(MPG)) %>%
  filter(Car_Make=="chevrolet")

#Q - 8

Car_Make <- c("amc", "audi", "bmw", "buick", "chevrolet", "datsun", "dodge", "ford")
First_Year <- c(1954, 1910, 1916, 1903, 1911, 1931, 1900, 1903)
df_auto_start <- data.frame(Car_Make=Car_Make, First_Year=First_Year, stringsAsFactors = FALSE)

df_auto_hold <- df_auto %>%
  left_join(df_auto_start, by="Car_Make")

sum(is.na(df_auto_hold$First_Year))

