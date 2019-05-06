library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(glm2)
library(popbio)

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
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) -> preData
  # mutate(MonthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[Month]) -> preData

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

# Plot
# h <- hist(logit_res, breaks=100)
# h_x_fit <- seq(min(logit_res), max(logit_res), length = 40)
# h_y_fit <- dnorm(h_x_fit, mean=mean(logit_res), sd=sd(logit_res))
# h_y_fit <- h_y_fit * diff(h$mids[1:2] * length(logit_res))
# lines(h_x_fit, h_y_fit, col="blue", lwd=3)



<<<<<<< HEAD
tomorrow %>% filter(RainTomorrow == "No") -> tomorrowNo
tomorrowNo[1:1000, ] -> tomorrowNo

tomorrow %>% filter(RainTomorrow == "Yes") -> tomorrowYes
tomorrowYes[1:1000, ] -> tomorrowYes

tomorrow <- rbind(tomorrowYes, tomorrowNo)

tomorrowYes_traning <- sample(nrow(tomorrowYes), .2*nrow(tomorrowYes))
tomorrowNo_traning <- sample(nrow(tomorrowNo), .2*nrow(tomorrowNo))
=======
>>>>>>> 48e99145e97248eeceba193d4720a84d57356f95

model <- glm(RainTomorrow ~ ., data = tomorrow, family = "binomial")
logit_res <-predict(model, tomorrow, type = "response")

res <- factor(ifelse(logit_res > 0.5,
                     "Yes","No"), 
              level = c("No","Yes"))

confusionMatrix(res, 
                tomorrow$RainTomorrow, 
                positive="No",
                mode="prec_recall")

# plot(RainTomorrow ~ ., data=tomorrow)
# lines(RainTomorrow ~ ., logit_res, lwd=10)

# plot(tomorrow, res)
# backward <- step(model)

plot(RainTomorrow ~ Month, data = tomorrow)
li
# lines(tomorrow$Month, logit_res)
