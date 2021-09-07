# Linear Regression on Seol Bike Counts
library(MASS)
library(tidyverse)

install.packages("corrplot")
library(corrplot)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

library(car)

# Objective: What is the relationship between the number of bikes rented with each of the predctor variables

bike <- readRDS("bike_clean.rds")
bike$hour <- as.factor(bike$hour)
attach(bike)

bike <- bike %>% filter(rent_count > 0) %>%
  group_by(open) %>%
  summarise(number = n())
pairs(bike[,4:15], main = "Bike Count Dataset", pch = 21)

model_1 <- lm(data = bike, rent_count ~ I(season) + I(holiday) + temperature + humidity + wind_speed + visibility + dewpoint_temp +
              solar_radiation + rainfall + snowfall)
summary(model_1)
 
# since the p-values of the visibility and dewpoint_temp are 0.159 and 0.123 respectively (both > 0.05), we can conclude that there is no significant
# relationship between bike rent count and these predictor variables. They will be excluded from the model

model_2 <- lm(data = bike, rent_count ~ season + holiday+ temperature + humidity + wind_speed +
                solar_radiation + rainfall + snowfall)

summary(model_2)

# The individual p-values indicate that the predictor variables are all highly significant. however, the multiple R-squared value is 0.478 which is low.
pairs(bike[,c(4,5,6,7,8,9,10,13,14,15)], main = "Bike Count Dataset", pch = 21)

# Residual plot
plot(model_2$fitted.values, rstandard(model_2), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = "Bike Count Model", pch = 20)
abline(h=0) #quadratic curve

# The residual plot displays a funnel shape. Hence, it shows unconstnt variance, trnasformation of data is required.

## Normal Probability Plot
qqnorm(rstandard(model_2), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_2), datax = TRUE)

# perform box cox transformation on the response
boxcox(model_2, lambda = seq(-2,2, by = 0.5), plotit = TRUE)
# choose lambda  = 0 or lambda  = 0.5
# transformed respose is log(y) oy sqrt(y)

model_3 <- lm(log(rent_count) ~ season + holiday+ temperature + humidity + wind_speed +
                    solar_radiation + rainfall + snowfall, data = bike)
summary(model_3)
plot(model_3$fitted.values, rstandard(model_3), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " Bike Rent Data Model 2", pch = 20)
abline(h=0)

qqnorm(rstandard(model_3), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_3), datax = TRUE)

################### Model 4 ###########################
model_4 <- lm(sqrt(rent_count) ~ season + holiday+ temperature + humidity + wind_speed +
                     solar_radiation + rainfall + snowfall, data = bike)
summary(model_4)

# Residual plot
plot(model_4$fitted.values, rstandard(model_4), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " wine Data Model 2", pch = 20)
abline(h=0)

## Normal Probability Plot
qqnorm(rstandard(model_4), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_4), datax = TRUE)


step(model_4, direction = "both")

############################# Model 5 ############################

model_5 <- lm(sqrt(rent_count) ~ season + holiday + temperature + humidity + wind_speed +
                solar_radiation + rainfall, data = bike)
summary(model_5)

# Residual plot
plot(model_5$fitted.values, rstandard(model_5), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " wine Data Model 2", pch = 20)
abline(h=0)

## Normal Probability Plot
qqnorm(rstandard(model_5), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_5), datax = TRUE)

########################### Model 6 ####################################

cor(bike[, -c("season", "holiday")])
model_6 <- lm(sqrt(rent_count) ~ season + holiday + temperature + humidity + wind_speed + humidity*visibility + humidity*solar_radiation +
                humidity*wind_speed + temperature*solar_radiation + wind_speed*I(hour) + solar_radiation + rainfall , data = bike)
summary(model_6)

# Residual plot
plot(model_6$fitted.values, rstandard(model_6), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " wine Data Model 2", pch = 20)
abline(h=0)

## Normal Probability Plot
qqnorm(rstandard(model_6), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_6), datax = TRUE)
       
bike_cont <- bike[,c(8,9,10,11,13,14,15)]

bike_cor <- cor(bike_cont)

corrplot(bike_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

step(model_6, direction = "both")
boxcox(model_6)


########################## model_7 #############################

model_7 <- lm(formula = log(rent_count)^2 ~ season + holiday + temperature + 
                humidity + wind_speed + visibility + solar_radiation + I(hour) + 
                rainfall + humidity:visibility + humidity:solar_radiation + 
                temperature:solar_radiation, data = bike)
summary(model_7)

# Residual plot
plot(model_7$fitted.values, rstandard(model_7), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " Wine Data Model 7", pch = 21)
abline(h=0)
abline(h=3, col = "blue", lty = "dashed")
abline(h=-3, col = "blue", lty = "dashed")


## Normal Probability Plot
qqnorm(rstandard(model_7), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_7), datax = TRUE)

boxcox(model_7, lambda = seq(-2,2, by = 0.5), plotit = TRUE)
step(model_7, direction = "both")
library(car)
boxTidwell(log(rent_count)^2 ~ season + holiday + temperature + 
             humidity + wind_speed + visibility + solar_radiation + I(hour) + 
             rainfall + humidity:visibility + humidity:solar_radiation + 
             temperature:solar_radiation, ~ season + holiday+ hour + temperature , data = bike)

chart.Correlation(bike_cont, histogram=TRUE, pch=19)
# In the above plot:
  
# The distribution of each variable is shown on the diagonal.
# On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
# On the top of the diagonal : the value of the correlation plus the significance level as stars
# Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “) 


######################## model 8 ###############################

model_8 <- lm(formula = log(rent_count)^2 ~ season + holiday + temperature + 
                humidity + wind_speed + visibility + solar_radiation + I(hour) + wind_speed*rainfall +
                wind_speed*snowfall + rainfall + humidity:visibility + humidity:solar_radiation + 
                temperature:solar_radiation, data = bike)
summary(model_8)

# Residual plot
plot(model_8$fitted.values, rstandard(model_8), xlab = "Fitted Values",
     ylab = "Standardized Residuals", main = " Wine Data Model 8", pch = 21)
abline(h=0)
abline(h=3, col = "blue", lty = "dashed")
abline(h=-3, col = "blue", lty = "dashed")


## Normal Probability Plot
qqnorm(rstandard(model_8), datax = TRUE, ylab = "Standardized Residuals", xlab = "z scores")
qqline(rstandard(model_8), datax = TRUE)

boxcox(model_8, lambda = seq(-2,2, by = 0.5), plotit = TRUE)
step(model_8, direction = "both")

boxTidwell(log(rent_count)^2 ~ season + holiday + temperature + 
             humidity + wind_speed + visibility + solar_radiation + I(hour) + 
             rainfall + humidity:visibility + humidity:solar_radiation + 
             temperature:solar_radiation, ~ season + holiday+ hour + temperature + rainfall + solar_radiation +humidity +wind_speed , data = bike)

apply(bike, 2, range)             
















#