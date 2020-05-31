library(tidyverse)
library(lubridate)
# library(rcartocolor)
library(zoo)

setwd("~/Dropbox/R coding/ottawa_beaches")
### CSV data:
# geographic data

# beaches count and beach status data
beaches <- read.csv("data/raw_data/beach_coliforms.csv", strip.white=TRUE)
opens <- read.csv("data/raw_data/beach_status.csv", strip.white=TRUE)

# turn into long format tables
beaches <- beaches %>%
  pivot_longer(names(beaches)[-1], names_to = "location", values_to = "count")
opens <- opens %>%
  pivot_longer(names(opens)[-1], names_to = "location", values_to = "status")

# Merge Ecoli counts and beach status data frames by date
beaches <- merge(beaches, opens, by = c('Date','location'), all=TRUE)
beaches$status <- factor(beaches$status,levels(beaches$status)[c(1,4,3,2)])
levels(beaches$status)[levels(beaches$status) == "NoSwim"] <- "E. coli"

# tidying data, especially dates
beaches$Date <- as.Date(beaches$Date)
beaches$location <- as.factor(beaches$location)
beaches <- beaches %>%
  dplyr::mutate(year = lubridate::year(beaches$Date),
                month = lubridate::month(beaches$Date),
                day = lubridate::day(beaches$Date),
                julian = lubridate::yday(beaches$Date))
beaches <-subset(beaches, select = c(Date, julian, year, month, day, location, count, status))

### WEATHER DATA:
# source: eg 2014: https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2011-12-14%7C2020-04-22&dlyRange=2011-12-15%7C2020-04-22&mlyRange=%7C&StationID=49568&Prov=ON&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=25&Line=14&searchMethod=contains&txtStationName=ottawa&timeframe=2&Day=22&Year=2014&Month=1#
# click download data csv. wget isn't working on in my terminal for some reason and homebrew...
# read in the weather data from ottawa airport for years 2014-2019
# grabbed 3 missing days from Ottawa CDA station (approximated mean)

weather <- data.frame(read.csv("data/raw_data/weather_data/weather2014.csv", strip.white = TRUE))
weather <- rbind(weather, data.frame(read.csv("data/raw_data/weather_data/weather2015.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("data/raw_data/weather_data/weather2016.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("data/raw_data/weather_data/weather2017.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("data/raw_data/weather_data/weather2018.csv", strip.white = TRUE)))
weather <- rbind(weather, data.frame(read.csv("data/raw_data/weather_data/weather2019.csv", strip.white = TRUE)))

#we can select desired columns from weather before we merge it with beaches df
# weather <- subset(weather, select = c(Date, Max.Temp...C., Mean.Temp...C., Min.Temp...C., Total.Rain..mm.,  Total.Snow..cm., Total.Precip..mm.,Year))
# change column names to make it less of a pita
# names(weather) <- c('date','Tmax', 'Tmean', 'Tmin', 'rain', 'snow', 'precip', 'year')

weather$Date <- as.Date(weather$Date)
weather <- weather %>%
  # dplyr::mutate(Date = lubridate::date(weather$Date))
  dplyr::mutate(julian = lubridate::yday(weather$Date))


### SUBSET WEATHER AND TAKE ONLY SUMMER OBSERVATIONS
weather <- subset(weather, select = c(Date, Tmax, Tmean, Tmin, rain))
names(weather) <- c('Date','Tmax','Tmean','Tmin','rain')
weather <- weather %>%
  filter(Date %in% beaches$Date)
beaches <- left_join(beaches, weather, by=c("Date","Date"))

write.csv(beaches, 'data/beaches.csv', row.names = FALSE)
# write.csv(weather,'data/weather_full.csv', row.names = FALSE)

rm(list=ls())
