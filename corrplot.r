library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)

rawData <- read.csv('csv/weatherAUS.csv')

#
#
# Data Preparation
#
#
# Cleaning
rawData %>%
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -Rainfall, -Location, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

# Prepocessing
preData %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) %>%
  mutate(MonthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[Month]) -> preData

preData$MinTemp <- as.numeric(preData$MinTemp)

# change yes/no to 1/0
preData$RainToday <- ifelse(preData$RainToday == 'Yes', 1, 0)
preData$RainTomorrow <- ifelse(preData$RainTomorrow == 'Yes', 1, 0)

# list columns
cols <- c('MinTemp', 'MaxTemp', 'WindGustSpeed', 'Sunshine', 'Evaporation')

for(col in c('WindSpeed', 'Cloud', 'Humidity', 'Pressure', 'Temp')) {
  for(time in c('9am', '3pm')) {
    cols <- c(cols, paste(col, time, sep = ''))
  }
}

# replace na with mean
for(col in cols) {
  preData[which(is.na(preData[, col])), col] <- mean(preData[, col], na.rm = TRUE)
}

preData -> data

#
# End Data Preparatiob
#

png('images/visualization/corrplot.png', 800, 600)
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