library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

data <- read.csv('csv/weatherAUS.csv')

#data <- data %>%
#  mutate(IsRain = ifelse(is.na(Rainfall), FALSE, (Rainfall > 0) ))

data <- data %>% 
        select(-Rainfall, -Location, -Date, -RainTomorrow, -RISK_MM)

set.seed(555)

test_index <- sample(nrow(data), .9*nrow(data))
training_data <- data[-test_index, ]
testing_data <- data[test_index, ]

tree <- rpart(RainToday ~ .,
              data = training_data)

rpart.plot(tree)

res_p <- predict(tree, training_data)
res_class <- predict(tree,
                    testing_data, 
                    type = "class")

confusionMatrix(res_class,
                testing_data$RainToday,
                positive = "Yes",
                mode = "prec_recall"
                )