library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(glm2)

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

# Preprocessing
preData %>%
  mutate(MonthName = format(as.Date(Date), format = '%b')) %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) -> preData

preData$MinTemp <- as.numeric(preData$MinTemp)

# change yes/no to 1/0
preData$RainToday <- ifelse(preData$RainToday == 'Yes', 1, 0)
preData$RainTomorrow <- ifelse(preData$RainTomorrow == 'Yes', 1, 0)

# replace na with mean
for(col in c('MinTemp', 'MaxTemp', 'WindGustSpeed', 'Sunshine', 'Evaporation')) {
  preData[which(is.na(preData[, col])), col] <- mean(preData[, col], na.rm = TRUE)
}

for(colname in c('WindSpeed', 'Cloud', 'Humidity', 'Pressure', 'Temp')) {
  for(time in c('9am', '3pm')) {
    col <- paste(colname, time, sep = '')
    preData[which(is.na(preData[, col])), col] <- mean(preData[, col], na.rm = TRUE)
  }
}

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
