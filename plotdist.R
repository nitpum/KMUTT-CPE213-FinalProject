library(tidyverse)

rawData <- read.csv('csv/weatherAUS.csv')

# Cleaning
rawData %>%
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

# View(preData)

cols <- c('MinTemp', 'MaxTemp', 'WindGustSpeed', 'Sunshine', 'Evaporation')
color <- c("#ff0000",
           "#ff7f00",
           "#ffff00",
           "#00ff00",
           "#00ffff",
           "#0000ff",
           "#8b00ff",
           "#ff0000",
           "#ff7f00",
           "#ffff00",
           "#00ff00",
           "#00ffff",
           "#0000ff",
           "#8b00ff",
           "#ff0000")

for(col in c('WindSpeed', 'Cloud', 'Humidity', 'Pressure', 'Temp')) {
  for(time in c('9am', '3pm')) {
    cols <- c(cols, paste(col, time, sep = ''))
  }
}

# par(mfrow=c(4, 4))
i <- 1
for(col in cols) {
  min(preData[, col], na.rm = TRUE)
  preData %>%
    subset(!is.na(preData[, col])) %>%
    ggplot() +
      geom_bar(
        aes_string(x = col),
        fill = color[i]
      ) +
      theme(text = element_text(size = 24)) +
      ggsave(paste('images/dist/', col, '.png', sep = ''))
  i <- i+1
}