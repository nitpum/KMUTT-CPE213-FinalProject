library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(glm2)
library(lattice)
library(scatterplot3d)

preData <- read.csv('csv/weatherAUS.csv')

#
#
# Data Preparation
#
#
preData %>%
  # Cleaning
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -Rainfall, -Location, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

# Prepocessing
preData %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) %>%
  mutate(MonthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[Month]) -> preData

preData$MinTemp <- as.numeric(preData$MinTemp)
preData$MinTemp[which(is.na(preData$MinTemp))] <- mean(preData$MinTemp,na.rm = TRUE)
preData$MaxTemp[which(is.na(preData$MaxTemp))] <- mean(preData$MaxTemp,na.rm = TRUE)
preData$WindGustSpeed[which(is.na(preData$WindGustSpeed))] <- mean(preData$WindGustSpeed,na.rm = TRUE)
preData$WindSpeed9am[which(is.na(preData$WindSpeed9am))] <- mean(preData$WindSpeed9am,na.rm = TRUE)
preData$WindSpeed3pm[which(is.na(preData$WindSpeed3pm))] <- mean(preData$WindSpeed3pm,na.rm = TRUE)
preData$Cloud3pm[which(is.na(preData$Cloud3pm))] <- mean(preData$Cloud3pm,na.rm = TRUE)
preData$Cloud9am[which(is.na(preData$Cloud9am))] <- mean(preData$Cloud9am,na.rm = TRUE)
preData$Humidity3pm[which(is.na(preData$Humidity3pm))] <- mean(preData$Humidity3pm,na.rm = TRUE)
preData$Humidity9am[which(is.na(preData$Humidity9am))] <- mean(preData$Humidity9am,na.rm = TRUE)
preData$Pressure9am[which(is.na(preData$Pressure9am))] <- mean(preData$Pressure9am,na.rm = TRUE)
preData$Pressure3pm[which(is.na(preData$Pressure3pm))] <- mean(preData$Pressure3pm,na.rm = TRUE)
preData$Temp9am[which(is.na(preData$Temp9am))] <- mean(preData$Temp9am,na.rm = TRUE)
preData$Temp3pm[which(is.na(preData$Temp3pm))] <- mean(preData$Temp3pm,na.rm = TRUE)
preData$Sunshine[which(is.na(preData$Sunshine))] <- mean(preData$Sunshine, na.rm = TRUE)
preData$Evaporation[which(is.na(preData$Evaporation))] <- mean(preData$Evaporation, na.rm = TRUE)

preData -> data

#
# End Data Preparatiob
#

tomorrow <- data

set.seed(555)

# Training data

# Predict Tomorrow

euei <- tomorrow$RainTomorrow == "Yes"
sum(euei, na.rm = TRUE)
euei <- tomorrow$RainTomorrow == "No"
sum(euei, na.rm = TRUE)

# tomorrow %>% filter(!is.na(RainTomorrow)) -> tomorrow

tomorrow %>% filter(RainTomorrow == "No") -> tomorrowNo
tomorrowNo[1:100, ] -> tomorrowNo

tomorrow %>% filter(RainTomorrow == "Yes") -> tomorrowYes
tomorrowYes[1:100, ] -> tomorrowYes

tomorrow <- rbind(tomorrowNo, tomorrowYes)

# view(logit_res)
# h <- hist(tomorrow, breaks=100)
# h_x_fit <- seq(min(logit_res), max(logit_res), length = 40)
# h_y_fit <- dnorm(h_x_fit, mean=mean(logit_res), sd=sd(logit_res))
# h_y_fit <- h_y_fit * diff(h$mids[1:2] * length(logit_res))
# lines(h_x_fit, h_y_fit, col="blue", lwd=3)

pairs(~MinTemp + MaxTemp + Sunshine, data=tomorrow)

tomorrow %>% mutate(MeanTemp = (MaxTemp - MinTemp) / 2) -> tomorrow

scatterplot3d(tomorrow$Month, tomorrow$Day, tomorrow$MeanTemp, pch=4)

tomorrow %>% group_by(Month) %>% filter(RainToday == "Yes") %>% summarise(count = n())
tomorrow %>% group_by(Month) %>% filter(RainToday == "No") %>% summarise(count = n())

tomorrow %>% group_by(Month) %>% filter(RainTomorrow == "Yes") %>% summarise(count = n())
tomorrow %>% group_by(Month) %>% filter(RainTomorrow == "No") %>% summarise(count = n())

