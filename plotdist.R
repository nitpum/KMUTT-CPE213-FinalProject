library(tidyverse)

rawData <- read.csv('csv/weatherAUS.csv')

# Cleaning
rawData %>%
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

# View(preData)

cols <- c('MinTemp', 'MaxTemp', 'WindGustSpeed', 'Sunshine', 'Evaporation')

for(col in c('WindSpeed', 'Cloud', 'Humidity', 'Pressure', 'Temp')) {
  for(time in c('9am', '3pm')) {
    cols <- c(cols, paste(col, time, sep = ''))
  }
}

for(col in cols) {
  min(preData[, col], na.rm = TRUE)
  preData %>%
    subset(!is.na(preData[, col])) %>%
    ggplot() +
      geom_bar(
        aes_string(x = col)
      ) +
      ggsave(paste('images/dist/', col, '.png', sep = ''))
}