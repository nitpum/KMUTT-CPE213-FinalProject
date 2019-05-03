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
png('images/visualization/ratio_no-yes-na.png', 800, 600)
 data %>% 
   ggplot(aes(x = RainTomorrow, fill = RainTomorrow)) + geom_bar() 
dev.off()

# Humidity9am ~ RainTomorrow
png('images/visualization/raintomorrow_humidity9am.png', 800, 600)
  data %>% 
   ggplot(aes(x=RainTomorrow, y=Humidity9am, colour = RainTomorrow, fill= RainTomorrow)) + 
   geom_violin()
dev.off()

# Humidity3pm ~ RainTomorrow
png('images/visualization/raintomorrow_humidity3pm.png', 800, 600)
  data %>% 
   ggplot(aes(x=RainTomorrow, y=Humidity3pm, colour = RainTomorrow, fill= RainTomorrow)) + 
   geom_violin()
dev.off()

number_of_record_by_location <- data %>%
  group_by(Location) %>%
  summarise(
    count = n()
  )

# rain today
png('images/visualization/rain_today_by_month.png', 800, 600)
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
    )
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
    )
dev.off()

# rain tomorrow by location scatter plot
png('images/visualization/rain_by_location_scatter.png', 1920, 1080)
data %>%
  mutate(
    RainTomorrow = ifelse(RainTomorrow == 'Yes', 1, 0)
  ) %>%
  ggplot() +
  geom_point(
    aes(x = Month, y = RainTomorrow, fill = Location)
  )
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