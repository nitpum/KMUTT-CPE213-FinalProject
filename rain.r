library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

data <- read.csv('csv/weatherAUS.csv')

#data <- data %>%
#  mutate(IsRain = ifelse(is.na(Rainfall), FALSE, (Rainfall > 0) ))
data$MinTemp<-as.numeric(data$MinTemp)

#Replacing NA and missing values with mean for numeric data

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
data$RainTomorrow[which(is.na(data$RainTomorrow))] <- mean(data$RainTomorrow, na.rm = TRUE)

tomorrow <- data %>% 
  select( -Rainfall, -Location, -Date, -RainToday, -RISK_MM)

data2 <- data %>% 
  select( -Rainfall, -Location, -Date, -RainTomorrow, -RISK_MM)

set.seed(555)

# Training data

# Model
model <- data

#model <- lm(RainTomorrow ~ Location, 
#            model)

# Predict Tomorrow

test_index <- sample(nrow(tomorrow), .2*nrow(tomorrow))
training_data <- tomorrow[-test_index, ]
testing_data <- tomorrow[test_index, ]

tree <- rpart(RainTomorrow ~ .,
              data = training_data)

rpart.plot(tree)

res_p <- predict(tree, training_data)
res_class <- predict(tree,
                    testing_data, 
                    type = "class")

confusionMatrix(res_class,
                testing_data$RainTomorrow,
                positive = "Yes",
                mode = "prec_recall"
                )