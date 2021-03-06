library(tidyverse)

rawData <- read.csv('csv/weatherAUS.csv')

#
#
# Data Preparation
#
#

# Cleaning
rawData %>%
  select(-WindDir3pm, 
          -WindDir9am, 
          -WindGustDir, 
          -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), 
         !is.na(RainToday)) 
          -> preData

# Preprocessing
preData %>%
  mutate(MonthName = format(as.Date(Date), format = '%b')) %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) -> preData

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