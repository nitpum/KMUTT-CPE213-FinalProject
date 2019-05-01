library(tidyverse)

cleveland <- read.csv('processed.cleveland.data', header = FALSE)
hungarian <- read.csv('processed.hungarian.data', header = FALSE)
switzerland <- read.csv('processed.switzerland.data', header = FALSE)
va <- read.csv('processed.va.data', header = FALSE)

header <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")

names(cleveland) <- header
names(hungarian) <- header
names(switzerland) <- header
names(va) <- header

cleveland <- cleveland %>% mutate('location' = 'cleveland')
cleveland[cleveland$age == "?"] = NULL
cleveland[cleveland$sex == "?"] = NULL
cleveland[cleveland$cp == "?"] = NULL
cleveland[cleveland$trestbps == "?"] = NULL
cleveland[cleveland$chol == "?"] = NULL
cleveland[cleveland$fbs == "?"] = NULL
cleveland[cleveland$restecg == "?"] = NULL
cleveland[cleveland$thalach == "?"] = NULL
cleveland[cleveland$exang == "?"] = NULL
cleveland[cleveland$oldpeak == "?"] = NULL
cleveland[cleveland$slope == "?"] = NULL
cleveland[cleveland$ca == "?"] = NULL
cleveland[cleveland$thal == "?"] = NULL
cleveland[cleveland$target == "?"] = NULL


hungarian <- hungarian %>%  mutate('location' = 'hungarian')
switzerland <- switzerland %>%  mutate('location' = 'switzerland')
va <- va %>% mutate('location' = 'va')

#data <- bind_rows(cleveland, hungarian)
