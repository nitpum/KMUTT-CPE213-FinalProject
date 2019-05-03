library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)

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

preData$RainToday<-str_replace_all(preData$RainToday,"No","0")
preData$RainToday<-str_replace_all(preData$RainToday,"Yes","1")
preData$RainTomorrow<-str_replace_all(preData$RainTomorrow,"No","0")
preData$RainTomorrow<-str_replace_all(preData$RainTomorrow,"Yes","1")
preData$MinTemp <- as.numeric(preData$MinTemp)
preData$RainToday<-as.numeric(preData$RainToday)
preData$RainTomorrow<-as.numeric(preData$RainTomorrow)
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

png('images/corrplot/corrplot.png', 800, 600)
  data %>%
    select(-MonthName) %>% 
    cor() %>%
    corrplot()
dev.off()

#for(location in unique(preData$Location)) {
#  png(paste('images/corrplot/', location, '.png', sep = ''), 800, 600)
#  preData %>%
#    filter(Location == location) %>%
#    select(-Location) %>%
#    cor() %>%
#    corrplot()
#  dev.off()
#}