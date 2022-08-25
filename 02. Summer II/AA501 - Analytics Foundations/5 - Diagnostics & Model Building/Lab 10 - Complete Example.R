library(ggplot2)
library(AmesHousing)
library(tidyverse)
library(car)
library(GGally)
library(nortest)

set.seed(123)

ames <- make_ordinal_ames()
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')
ames_num <- train[sapply(train, is.numeric)]

correlation <- round(cor(ames_num), 2)
correlation[(correlation < 0.7 & correlation > -0.7) | correlation == 1] <- ""
# Gr_Liv_Area - TotRms_AbvGrd:  0.81
# Garage_Area - Garage_Cars: 0.89
# First_Flr_SF - Total_Bsmt_SF: 0.81
# remove TotRms_AbvGrd, Garage_Area, Total_Bsmt_SF

train <- subset(train, select = -c(id, TotRms_AbvGrd, Garage_Area, Total_Bsmt_SF, Gr_Liv_Area, Second_Flr_SF, BsmtFin_Type_1, BsmtFin_Type_2, Garage_Finish, Bsmt_Qual, Misc_Feature, Garage_Qual, MS_Zoning, Exterior_2nd, MS_SubClass))

# Checking for collinearity
collin_test = lm(Sale_Price ~ ., data = train)
vif(collin_test)

# dropping 1 na value so backward selection works
omit_na_data <- na.omit(train)
alpha = 0.0003
p_val = qchisq(alpha, 1, lower.tail = FALSE)

full_model <- lm(Sale_Price ~ ., data = omit_na_data)
empty_model <- lm(Sale_Price ~ 1, data = omit_na_data)

# BACKWARDS SELECTION
back_model <- step(full_model,
                   scope = list(lower = empty_model,
                                upper = full_model),
                   direction = "backward", k = p_val) 

# Step:  AIC=42465.24
# Sale_Price ~ Lot_Area + Neighborhood + Bldg_Type + House_Style + 
#     Overall_Qual + Overall_Cond + Year_Built + Roof_Matl + Bsmt_Exposure + 
#     Bsmt_Unf_SF + First_Flr_SF + Bsmt_Full_Bath + Full_Bath + 
#     Half_Bath + Fireplaces + Garage_Cars + Screen_Porch + Pool_QC + 
#     Misc_Val + Sale_Condition

# FORWARD SELECTION
forward_model <- step(full_model,
                   scope = list(lower = empty_model,
                                upper = full_model),
                   direction = "forward", k = p_val)

# Start:  AIC=43919.56
# Sale_Price ~ Lot_Frontage + Lot_Area + Street + Alley + Lot_Shape + 
#     Land_Contour + Utilities + Lot_Config + Land_Slope + Neighborhood + 
#     Condition_1 + Condition_2 + Bldg_Type + House_Style + Overall_Qual + 
#     Overall_Cond + Year_Built + Year_Remod_Add + Roof_Style + 
#     Roof_Matl + Exterior_1st + Mas_Vnr_Type + Mas_Vnr_Area + 
#     Exter_Qual + Exter_Cond + Foundation + Bsmt_Cond + Bsmt_Exposure + 
#     BsmtFin_SF_1 + BsmtFin_SF_2 + Bsmt_Unf_SF + Heating + Heating_QC + 
#     Central_Air + Electrical + First_Flr_SF + Low_Qual_Fin_SF + 
#     Bsmt_Full_Bath + Bsmt_Half_Bath + Full_Bath + Half_Bath + 
#     Bedroom_AbvGr + Kitchen_AbvGr + Kitchen_Qual + Functional + 
#     Fireplaces + Fireplace_Qu + Garage_Type + Garage_Cars + Garage_Cond + 
#     Paved_Drive + Wood_Deck_SF + Open_Porch_SF + Enclosed_Porch + 
#     Three_season_porch + Screen_Porch + Pool_Area + Pool_QC + 
#     Fence + Misc_Val + Mo_Sold + Year_Sold + Sale_Type + Sale_Condition + 
#     Longitude + Latitude

# STEPWISE SELECTION
stepwise_model <- step(full_model,
                      scope = list(lower = empty_model,
                                   upper = full_model),
                      direction = "both", k = p_val)

# Step:  AIC=42465.24
# Sale_Price ~ Lot_Area + Neighborhood + Bldg_Type + House_Style + 
#     Overall_Qual + Overall_Cond + Year_Built + Roof_Matl + Bsmt_Exposure + 
#     Bsmt_Unf_SF + First_Flr_SF + Bsmt_Full_Bath + Full_Bath + 
#     Half_Bath + Fireplaces + Garage_Cars + Screen_Porch + Pool_QC + 
#     Misc_Val + Sale_Condition

best_model <- lm(Sale_Price ~ Lot_Area + Neighborhood + Bldg_Type + House_Style +
                     Overall_Qual + Overall_Cond + Year_Built + Roof_Matl + Bsmt_Exposure +
                     Bsmt_Unf_SF + First_Flr_SF + Bsmt_Full_Bath + Full_Bath +
                     Half_Bath + Fireplaces + Garage_Cars + Screen_Porch + Pool_QC +
                     Misc_Val + Sale_Condition, data = omit_na_data)

ggplot(best_model, aes(x = fitted.values(best_model), y = resid(best_model))) +
    geom_point()

root_model <- lm((Sale_Price ^ 0.25) ~ Overall_Qual + First_Flr_SF + Half_Bath +
                     Garage_Cars +  Full_Bath  + Bldg_Type + Roof_Matl  +
                     Lot_Area + Year_Remod_Add + Fireplaces + Mas_Vnr_Area +
                     Screen_Porch + Exter_Qual + Bsmt_Unf_SF  + Sale_Condition +
                     Pool_QC + Bedroom_AbvGr, data = omit_na_data)

# Check for normality
qqnorm(resid(root_model))
qqline(resid(root_model))

hist(resid(root_model))

# check for variance
ggplot(root_model, aes(x = fitted.values(root_model), y = resid(root_model))) +
    geom_point()

ggplot(root_model, aes(x = log(First_Flr_SF), y = resid(root_model))) +
    geom_point()

# get cutoff for hat values
coeff <- length(coef(root_model))
hat_cut <- 2 * (coeff) / nrow(omit_na_data)

# plots residuals and hat values
ggplot(root_model, aes(x = hatvalues(root_model), y = rstudent(root_model))) + 
    geom_point() +
    geom_hline(yintercept = 3) + 
    geom_vline(xintercept = hat_cut) + 
    labs(x = "Hat Values", y = "Residuals")

stud_resids <- rstudent(root_model)
stud_resids <- stud_resids[stud_resids > 3 | stud_resids < -3]
print(stud_resids)
print(length(stud_resids))

stud_hat <- hatvalues(root_model)
stud_hat <- stud_hat[stud_hat > hat_cut]
print(stud_dffit)
print(length(stud_dffit))
# Remove observations 1201, 1217, 1660

better_data <- omit_na_data[-c(1201, 1217, 1660),]

root_model <- lm((Sale_Price ^ 0.25) ~ Overall_Qual + First_Flr_SF + Half_Bath +
                     Garage_Cars +  Full_Bath  + Bldg_Type +
                     Lot_Area + Year_Remod_Add + Fireplaces + Mas_Vnr_Area +
                     Screen_Porch + Exter_Qual + Bsmt_Unf_SF  + Sale_Condition +
                     Pool_QC + Bedroom_AbvGr, data = better_data)

# Check for normality
qqnorm(resid(root_model))
qqline(resid(root_model))

# get cutoff for hat values
coeff <- length(coef(root_model))
hat_cut <- 2 * (coeff) / nrow(better_data)
cookd_cut <- 4 / (nrow(better_data) - coeff - 1)

n_index <- nrow(better_data)
index_table <- cbind(better_data, n_index)

# plots residuals and hat values
ggplot(root_model, aes(x = hatvalues(root_model), y = rstudent(root_model))) + 
    geom_point() +
    geom_hline(yintercept = 3) + 
    geom_hline(yintercept = -3) + 
    geom_vline(xintercept = hat_cut) + 
    labs(x = "Hat Values", y = "Residuals")

stud_resids <- rstudent(root_model)
stud_resids <- stud_resids[stud_resids > 3 | stud_resids < -3]
print(stud_resids)
print(length(stud_resids))

infl_obs <- hatvalues(root_model)
infl_obs <- infl_obs[infl_obs > hat_cut]
print(infl_obs)
print(length(infl_obs))
# 33, 66, 453, 719, 1172, 1391, 1471, 1473, 1534, 1651

infl_obs <- cooks.distance(root_model)
infl_obs <- infl_obs[infl_obs > cookd_cut]
print(infl_obs[infl_obs > 1])
print(length(infl_obs))

valid_fit <- predict(root_model, newdata = test)
MAE <- mean(abs(test$Sale_Price - (valid_fit ^ 4)))
# MAE: 20150.84
summary(root_model)
# adj r^2: 0.8727 
# r^2: 0.8749

best_data <- better_data[-c(33, 66, 453, 719, 1172, 1391, 1471, 1473, 1534, 1651),]

root_model <- lm((Sale_Price ^ 0.25) ~ Overall_Qual + First_Flr_SF + Half_Bath +
                     Garage_Cars +  Full_Bath  + Bldg_Type + Year_Sold +
                     Lot_Area + Year_Remod_Add + Fireplaces + Mas_Vnr_Area +
                     Screen_Porch + Exter_Qual + Bsmt_Unf_SF  + Sale_Condition +
                     Pool_QC + Bedroom_AbvGr, data = best_data)

# Check for normality
qqnorm(resid(root_model))
qqline(resid(root_model))

coeff <- length(coef(root_model))
hat_cut <- 2 * (coeff) / nrow(better_data)
cookd_cut <- 4 / (nrow(better_data) - coeff - 1)

ggplot(root_model, aes(x = hatvalues(root_model), y = rstudent(root_model))) + 
    geom_point() +
    geom_hline(yintercept = 3) + 
    geom_hline(yintercept = -3) + 
    geom_vline(xintercept = hat_cut) + 
    labs(x = "Hat Values", y = "Residuals")

stud_resids <- rstudent(root_model)
stud_resids <- stud_resids[stud_resids > 3 | stud_resids < -3]
print(stud_resids)
print(length(stud_resids))

infl_obs <- hatvalues(root_model)
infl_obs <- infl_obs[infl_obs > hat_cut]
print(infl_obs)
print(length(infl_obs))

ggplot(root_model, aes(x = fitted.values(root_model), y = resid(root_model))) +
    geom_point()

valid_fit <- predict(root_model, newdata = test)
MAE <- mean(abs(test$Sale_Price - (valid_fit ^ 4)))
# MAE: 19988.70
summary(root_model)
# adj r^2: 0.8928 
# r^2: 0.8947

