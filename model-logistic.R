library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(glm2)

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

euei <- tomorrow$RainTomorrow == "Yes"
sum(euei, na.rm = TRUE)
euei <- tomorrow$RainTomorrow == "No"
sum(euei, na.rm = TRUE)

# tomorrow %>% filter(!is.na(RainTomorrow)) -> tomorrow

tomorrow %>% filter(RainTomorrow == "No") -> tomorrowNo
# tomorrowNo[1:30000, ] -> tomorrowNo

tomorrow %>% filter(RainTomorrow == "Yes") -> tomorrowYes
# tomorrowYes[1:30000, ] -> tomorrowYes

tomorrowYes_traning <- sample(nrow(tomorrowYes), .2*nrow(tomorrowYes))
tomorrowNo_traning <- sample(nrow(tomorrowNo), .2*nrow(tomorrowNo))

model <- glm(RainTomorrow ~ ., data = tomorrow, family = "binomial")
logit_res <-predict(model, tomorrow, tpye="response")
# view(logit_res)
hist(logit_res, breaks=100)

res <- factor(ifelse(logit_res > 0.5,
                     "Yes","No"), 
              level = c("No","Yes"))

confusionMatrix(res, 
                tomorrow$RainTomorrow, 
                positive="No",
                mode="prec_recall")
