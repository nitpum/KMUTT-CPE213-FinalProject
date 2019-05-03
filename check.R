library(tidyverse)

rawData <- read.csv('csv/weatherAUS.csv')

# Cleaning
rawData %>%
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

View(preData)

for(col in c('Evaporation', 'Sunshine', 'Cloud9am', 'Cloud3pm')) {
  min(preData[, col], na.rm = TRUE)
  preData %>%
    subset(!is.na(preData[, col])) %>%
    ggplot() +
      geom_bar(
        aes_string(x = col)
      ) +
      ggsave(paste('images/dist/', col, '.png', sep = ''))
}