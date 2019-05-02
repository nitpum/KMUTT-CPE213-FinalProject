library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

field <- c(
  'age',
  'sex',
  'cp',
  'trestbps',
  'chol',
  'fbs',
  'restecg',
  'thalach',
  'exang',
  'oldpeak',
  'slope',
  'ca',
  'thal',
  'target',
  'location'
)

# read data from csv
cleveland <- read.csv('processed.cleveland.data', header = FALSE)
hungarian <- read.csv('processed.hungarian.data', header = FALSE)
switzerland <- read.csv('processed.switzerland.data', header = FALSE)
va <- read.csv('processed.va.data', header = FALSE, stringsAsFactors = FALSE)

# set location
cleveland$V15 <- 'cleveland'
hungarian$V15 <- 'hungarian'
switzerland$V15 <- 'switzerland'
va$V15 <- 'va'

# stack data
data <- rbind(cleveland, hungarian, switzerland, va)

# set column name
colnames(data) <- field

# change '?' to NA
data[data == '?'] <- NA

# change column 1 to 14 to number
for(i in 1:14) {
  data[, i] <- as.numeric(data[, i])
}

# remove NA
data <- data[complete.cases(data), ]

# change target to positive/negative
data <- data %>%
  mutate(target = ifelse(data$target == 1, 'positive', 'negative'))

predictData <- data %>%
  select(
    -location
  )

set.seed(420)

# sample indexes for training and testing
testIndex <- sample(
  nrow(predictData),
  .3 * nrow(predictData)
)

trainingData <- predictData[-testIndex, ]
testingData <- predictData[testIndex, ]

tree <- rpart(target ~ ., data = trainingData)
rpart.plot(tree)

res_p <- predict(tree, testingData)