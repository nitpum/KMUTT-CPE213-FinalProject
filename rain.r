library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)

data <- read.csv('csv/weatherAUS.csv')


#
#
# Data preparation
#
#
#Replacing NA and missing values with mean for numeric data
data$MinTemp <- as.numeric(data$MinTemp)
data$RainToday<-as.numeric(data$RainToday)
data$RainTomorrow<-as.numeric(data$RainTomorrow)
data$MinTemp[which(is.na(data$MinTemp))] <- mean(data$MinTemp,na.rm = TRUE)
data$MaxTemp[which(is.na(data$MaxTemp))] <- mean(data$MaxTemp,na.rm = TRUE)
data$Rainfall[which(is.na(data$Rainfall))] <- mean(data$Rainfall,na.rm = TRUE)
data$WindGustSpeed[which(is.na(data$WindGustSpeed))] <- mean(data$WindGustSpeed,na.rm = TRUE)
data$WindSpeed9am[which(is.na(data$WindSpeed9am))] <- mean(data$WindSpeed9am,na.rm = TRUE)
data$WindSpeed3pm[which(is.na(data$WindSpeed3pm))] <- mean(data$WindSpeed3pm,na.rm = TRUE)
data$Humidity3pm[which(is.na(data$Humidity3pm))] <- mean(data$Humidity3pm,na.rm = TRUE)
data$Humidity9am[which(is.na(data$Humidity9am))] <- mean(data$Humidity9am,na.rm = TRUE)
data$Pressure9am[which(is.na(data$Pressure9am))] <- mean(data$Pressure9am,na.rm = TRUE)
data$Pressure3pm[which(is.na(data$Pressure3pm))] <- mean(data$Pressure3pm,na.rm = TRUE)
data$Temp9am[which(is.na(data$Temp9am))] <- mean(data$Temp9am,na.rm = TRUE)
data$Temp3pm[which(is.na(data$Temp3pm))] <- mean(data$Temp3pm,na.rm = TRUE)
data$Sunshine[which(is.na(data$Sunshine))] <- mean(data$Sunshine, na.rm = TRUE)
data$Evaporation[which(is.na(data$Evaporation))] <- mean(data$Evaporation, na.rm = TRUE)
data$RainToday[which(is.na(data$RainToday))] <- mean(data$RainToday, na.rm = TRUE)
data$RainTomorrow[which(is.na(data$RainTomorrow))] <- mean(data$RainTomorrow, na.rm = TRUE)

# Preprocessing Data
predata <- data %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>% 
  select( -Rainfall, -RainToday, -RISK_MM)

# Sydney <- data %>% filter(
#   Location == 'Sydney' &
#   grepl("\\d{4}-0[2-6]-\\d{2}", Date)
# )
# Brisbane <- data %>% filter(
#   Location == 'Brisbane' &
#   grepl("\\d{4}-(0[1-3]|12)-\\d{2}", Date)
# )
# Perth <- data %>% filter(
#   Location == 'Perth' &
#   grepl("\\d{4}-(0[5-8])-\\d{2}", Date)
# )
# Darwin <- data %>% filter(
#   Location == 'Darwin' &
#   grepl("\\d{4}-(0[1-4]|1[1-2])-\\d{2}", Date)
# )

# data <- rbind(Sydney, Brisbane, Perth, Darwin)

# data <- predata %>% filter(Location == 'Adelaide')

tomorrow <- data %>% 
  select( -Rainfall, -Date, -RainToday, -RISK_MM)

# data2 <- data %>% 
#   select( -Rainfall, -Location, -Date, -RainTomorrow, -RISK_MM)

set.seed(555)

