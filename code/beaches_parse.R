library(tidyverse)
library(lubridate)
require(rcartocolor)

### CSV data:

# beaches count and beach status data
beaches <- read.csv("beach_ecoli.csv", strip.white=TRUE)
opens <- read.csv("beach_status.csv", strip.white=TRUE)

# Merge Ecoli counts and beach status data frames by date
beaches <- beaches %>%
  pivot_longer(names(beaches)[-1], names_to = "location", values_to = "count")
opens <- opens %>%
  pivot_longer(names(opens)[-1], names_to = "location", values_to = "status")
status<-as.factor(opens$status)
beaches$status <- factor(status,levels(status)[c(1,4,3,2)])
rm(opens)

# tidying data, especially dates
beaches$date <- as.Date(beaches$Date)
beaches <- beaches %>%
  dplyr::mutate(year = lubridate::year(beaches$Date),
                month = lubridate::month(beaches$Date),
                day = lubridate::day(beaches$Date),
                julian = lubridate::yday(beaches$Date))
beaches$year <- as.factor(beaches$year)
beaches <-subset(beaches, select = c(date, julian, year, location, count, status))
write.csv(beaches, 'beach_data.csv', row.names = FALSE)
# beaches[is.na(beaches)] <- 1



### WEATHER DATA:
# source: eg 2014: https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2011-12-14%7C2020-04-22&dlyRange=2011-12-15%7C2020-04-22&mlyRange=%7C&StationID=49568&Prov=ON&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=25&Line=14&searchMethod=contains&txtStationName=ottawa&timeframe=2&Day=22&Year=2014&Month=1#
# click download data csv. wget isn't working on in my terminal for some reason and homebrew...yea more like homepoo if you ask me.
# read in the weather data from ottawa airport for years 2014-2019

weather <- data.frame(read.csv("raw_data/weather_data/weather2014.csv", strip.white = TRUE))
weather <- rbind(weather, data.frame(read.csv("raw_data/weather_data/weather2015.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("raw_data/weather_data/weather2016.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("raw_data/weather_data/weather2017.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("raw_data/weather_data/weather2018.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("raw_data/weather_data/weather2019.csv", strip.white = TRUE)))
#we can select desired columns from weather before we merge it with beaches df
weather <- subset(weather, select = c(Date.Time, Max.Temp...C., Mean.Temp...C., Min.Temp...C., Total.Rain..mm.,  Total.Snow..cm., Total.Precip..mm.,Year))
# change column names to make it less of a pita
names(weather) <- c('date','Tmax', 'Tmean', 'Tmin', 'rain', 'snow', 'precip', 'year')
weather$date <- as.Date(weather$date)
weather <- weather %>%
  dplyr::mutate(julian = lubridate::yday(weather$date))
write.csv(weather,'weather_full.csv', row.names = FALSE)

### SUBSET WEATHER AND TAKE ONLY SUMMER OBSERVATIONS
weather <- subset(weather, select = c(date, Tmax, Tmean, Tmin, rain))
names(weather) <- c('date','Tmax','Tmean','Tmin','rain')
weather <- weather %>%
  filter(date %in% beaches$date)
beaches <- left_join(beaches, weather, by=c("date","date"))

write.csv(beaches, 'beach_data.csv', row.names = FALSE)
rm(list=ls())
