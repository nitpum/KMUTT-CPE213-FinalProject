library(tidyverse)

rawData <- read.csv('csv/weatherAUS.csv')

#
#
# Data Preparation
#
#
# Cleaning
rawData %>%
  select(-WindDir3pm, -WindDir9am, -WindGustDir, -RISK_MM) %>% 
  filter(!is.na(RainTomorrow), !is.na(RainToday)) -> preData

# Preprocessing
preData %>%
  mutate(MonthName = format(as.Date(Date), format = '%b')) %>%
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day)) -> preData

preData$MinTemp <- as.numeric(preData$MinTemp)

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

# # Ratio No:Yes:NA in RainTomorrow
png('images/visualization/ratio_no-yes-na.png', 1920, 1080)
 data %>% 
    ggplot(aes(x = RainTomorrow, fill = RainTomorrow)) + 
    geom_bar() +
    theme(text = element_text(size = 48))
dev.off()

# Humidity9am ~ RainTomorrow
png('images/visualization/raintomorrow_humidity9am.png', 1920, 1080)
  data %>% 
    ggplot(aes(x=RainTomorrow, y=Humidity9am, colour = RainTomorrow, fill= RainTomorrow)) + 
    geom_violin() +
    theme(text = element_text(size = 48))
dev.off()

# Humidity3pm ~ RainTomorrow
png('images/visualization/raintomorrow_humidity3pm.png', 1920, 1080)
  data %>% 
    ggplot(aes(x=RainTomorrow, y=Humidity3pm, colour = RainTomorrow, fill= RainTomorrow)) + 
    geom_violin() +
    theme(text = element_text(size = 48))
dev.off()

number_of_record_by_location <- data %>%
  group_by(Location) %>%
  summarise(
    count = n()
  )

# rain today
png('images/visualization/rain_today_by_month.png', 1920, 1080)
data %>%
  mutate(
    RainToday = ifelse(RainToday == 'Yes', 1, 0)
  ) %>%
  group_by(Month) %>%
  summarise(
    mean = mean(RainToday)
  ) %>%
  ggplot() +
    geom_bar(
      aes(x = Month, y = mean),
      stat = 'summary', fun.y = 'mean'
    ) +
    theme(text = element_text(size = 48))
dev.off()

# rain today by location
png('images/visualization/rain_by_location.png', 1920, 1080)
data %>%
  mutate(
    RainToday = ifelse(RainToday == 'Yes', 1, 0)
  ) %>%
  group_by(Location, Month) %>%
  summarise(
    mean = mean(RainToday)
  ) %>%
  ggplot() +
    geom_bar(
      aes(x = Month, y = mean, fill = Location),
      position = 'dodge', stat = 'summary', fun.y = 'mean'
    ) +
    theme(text = element_text(size = 48))
dev.off()

# humidity3pm rainfall
png('images/visualization/humidity3pm_rainfall.png', 1920, 1080)
data %>%
  filter(Rainfall < 200) %>% 
  ggplot(aes(x=Humidity3pm, y = Rainfall)) + 
  geom_point(aes(colour = Rainfall)) + geom_smooth(method = "lm") +
  theme(text = element_text(size = 48))
dev.off()

# Pair
png('images/visualization/pairs.png', 1920, 1080)
data %>% 
  select(contains("Temp"), Rainfall, contains("Spedd"), 
         contains("Humidity"), contains("Pressure")) -> pairs_data
pairs(pairs_data)
dev.off()
# # temp
# data %>%
#   group_by(Month, Day) %>%
#   summarise(
#     Temp3pm = mean(Temp3pm)
#   ) %>%
#   ggplot() +
#     geom_line(
#       aes(x = as.Date(paste(Month, Day, sep = '-'), format = '%m-%d'), y = Temp3pm)
#     )

# dist
# data %>%
#   ggplot() +
#     geom_bar(
#       aes(x = Humidity3pm)
#     )
