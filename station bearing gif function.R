## Capital Bikeshare
## Ride Bearings
## By Alex Freeman

library(tidyverse)
library(lubridate)
library(data.table)

setwd("~/R/Bikeshare")

####### DATA MUNGING #########

bike <- read_rds("~/R/Bikeshare/bike_full.rds")

bike_time <- bike %>%
  filter(start.station.number != end.station.number) %>%
  select(start.date, start.time,
         start.lat, start.long,
         end.lat, end.long,
         start.station, start.station.number)

bike_time$start.hour <- hour(bike_time$start.time)

earth.bear <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  bear <- atan2(sin(dlon) * cos(b1), cos(a1) * sin(b1) - sin(a1) * 
                  cos(b1) * cos(dlon))
  deg <- (bear%%(2 * pi)) * (180/pi)
  return(deg)
}

bike_time$bike.bear <- round(earth.bear(bike_time$start.long,
                                        bike_time$start.lat,
                                        bike_time$end.long,
                                        bike_time$end.lat), 2)

bike_stations <- read_csv("~/R/Bikeshare/DC Bikeshare Locations.csv")

bike_stations <- bike_stations %>%
  select(id = ID,
         station = ADDRESS) %>%
  as.data.table()

top_station <- bike_time %>%
  group_by(start.station) %>%
  summarise(rides = length(start.station)) %>%
  arrange(desc(rides))

## FUNCTION for station plot

station_bear <- function(station) {
  
  station_hour_direction <- bike_time %>%
    filter(start.station == station) %>%
    group_by(start.hour) %>%
    summarise(start.lat = mean(start.lat, na.rm = TRUE),
              start.long = mean(start.long, na.rm = TRUE),
              end.lat = mean(end.lat, na.rm = TRUE),
              end.long = mean(end.long, na.rm = TRUE),
              count = length(start.station)) %>%
    arrange(start.hour)
  
  station_hour_direction$bike.bear <- earth.bear(station_hour_direction$start.long,
                                                 station_hour_direction$start.lat,
                                                 station_hour_direction$end.long,
                                                 station_hour_direction$end.lat)
  
  
  for (i in c(0:23)) {
    
    ggplot(filter(bike_time, 
                  start.hour == i, start.station == station),
           aes(x = bike.bear)) +
      geom_histogram(bins = 88, alpha = .5) +
      geom_vline(data = filter(station_hour_direction, 
                               start.hour == i),
                 aes(xintercept = mean(bike.bear, na.rm = TRUE)),
                 color = "red", size = 2, alpha = .7) +
      scale_x_continuous(limits = c(0, 360), 
                         breaks=seq(0,360-1,by=45),
                         labels=c("N","N-E","E","S-E","S","S-W","W","N-W")) +
      coord_polar() +
      theme_minimal(base_size = 14) +
      labs(list(title = "Compass Bearing of DC Bikeshare Rides", x = "", y = "",
                subtitle = paste0(i, ":00 to ", i, ":59 - ", station),
                caption = paste("Data from www.capitalbikeshare.com
                              Total rides = ", station_hour_direction$count[i + 1]))) +
      theme(axis.text.y=element_blank())
    
    ggsave(paste0("images/medium bearing/", station, " GIF/bearing compass", 
                  i, ".png"),
           height = 15, width = 15, units = "cm")
    
    print(i)
    
  }
  
  
  
}

## Run station_bear function

## station name must be in quotes
## can also go by station id with any number:
## bike_stations[bike_stations$id == 8]$station

##### NEED TO MAKE FOLDER FOR THE STATION FIRST ####

# station_bear("Massachusetts Ave & Dupont Circle NW")

