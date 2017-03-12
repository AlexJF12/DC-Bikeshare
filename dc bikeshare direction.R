## Capital Bikeshare
## Ride Bearings
## By Alex Freeman

library(tidyverse)
library(lubridate)
library(data.table)
library(ggmap)

setwd("~/R/Bikeshare")

####### DATA MUNGING #########

bike <- read_rds("~/R/Bikeshare/bike_full.rds")

## Create bike data.table for GPS addition

bike_time <- bike %>%
  filter(start.station.number != end.station.number) %>%
  select(start.date, start.time,
         start.lat, start.long,
         end.lat, end.long,
         start.station, start.station.number)


bike_time$start.hour <- hour(bike_time$start.time)
bike_time$start.min <- minute(bike_time$start.time)

bike_time <- as.data.table(bike_time)

## Create 15 min chunks (for finer analysis)

bike_time$chunk[bike_time$start.min < 15] <- 1
bike_time$chunk[bike_time$start.min >= 15 & bike_time$start.min < 30] <- 2
bike_time$chunk[bike_time$start.min >= 30 & bike_time$start.min < 45] <- 3
bike_time$chunk[bike_time$start.min >= 45] <- 4

## create count of rides in each hour

bike_time <- bike_time[, n.chunk := .N, by = list(start.hour, chunk)]
bike_time <- bike_time[, n.hour := .N, by = start.hour]

## Bearing function found in the fossil package

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

## Create bearing of each ride

bike_time$bike.bear <- round(earth.bear(bike_time$start.long,
                                  bike_time$start.lat,
                                  bike_time$end.long,
                                  bike_time$end.lat), 2)

## Bearing of the mean ride

earth.bear(mean(bike$start.long, na.rm = TRUE),
           mean(bike$start.lat, na.rm = TRUE),
           mean(bike$end.long, na.rm = TRUE),
           mean(bike$end.lat, na.rm = TRUE))

## Create day, hour, chunk dataset
## Do I want median or mean GPS location?
## I went with mean

bike_direction <- bike_time %>%
  group_by(start.hour, chunk) %>%
  summarise(start.lat = mean(start.lat, na.rm = TRUE),
            start.long = mean(start.long, na.rm = TRUE),
            end.lat = mean(end.lat, na.rm = TRUE),
            end.long = mean(end.long, na.rm = TRUE),
            n = mean(n.chunk)) %>%
  arrange(start.hour, chunk)

bike_direction <- within(bike_direction, 
                         hour.chunk <- paste(start.hour, chunk, sep = "."))



bike_direction$bike.bear <- earth.bear(bike_direction$start.long,
                                       bike_direction$start.lat,
                                       bike_direction$end.long,
                                       bike_direction$end.lat)

bike_chunk_direction <- data.table(bike_direction)
rm(bike_direction)

write.csv(select(bike_chunk_direction, 
                 start.hour, chunk, bike.bear), 
          file = "chunk bearing.csv")

## Create bearing by hour dataset

bike_hour_direction <- bike_time %>%
  group_by(start.hour) %>%
  summarise(start.lat = mean(start.lat, na.rm = TRUE),
            start.long = mean(start.long, na.rm = TRUE),
            end.lat = mean(end.lat, na.rm = TRUE),
            end.long = mean(end.long, na.rm = TRUE),
            n = mean(n.hour)) %>%
  arrange(start.hour)

bike_hour_direction$bike.bear <- earth.bear(bike_hour_direction$start.long,
                                       bike_hour_direction$start.lat,
                                       bike_hour_direction$end.long,
                                       bike_hour_direction$end.lat)

write.csv(select(bike_hour_direction, start.hour, bike.bear), file = "hourly bearing.csv")


## bike_time bearings

bike_time_bear <- bike_time %>%
  select(start.lat, start.long, end.lat, end.long, start.hour, start.station)

bike_time_bear$bike.bear <- earth.bear(bike_time_bear$start.long,
                                       bike_time_bear$start.lat,
                                       bike_time_bear$end.long,
                                       bike_time_bear$end.lat)
  



##########



###### VIZ #######


## Viz total rides by bearing

ggplot(bike_time_bear, aes(x = bike.bear)) +
  geom_histogram(bins = 179) +
  scale_x_continuous(limits = c(0, 360), 
                     breaks=seq(0, 359, by=45),
                     labels=c("N","N-E","E","S-E","S","S-W","W","N-W")) +
  coord_polar() +
  theme_minimal(base_size = 14) +
  labs(list(title = "Bearing of all DC Bikeshare Rides", x = "", y = "", 
            caption = "Data from www.capitalbikeshare.com \n3.2 million rides")) +
  theme(axis.text.y=element_blank())

ggsave("images/medium bearing/bearing of rides.png",
       height = 15, width = 15, units = "cm")

## By hour, all bearings

hour <- 12

ggplot(filter(bike_time_bear, start.hour == hour),
              aes(x = bike.bear)) +
  geom_histogram(bins = 88, alpha = .5) +
  geom_vline(data = filter(bike_hour_direction, start.hour == hour), 
             aes(xintercept = bike.bear),
             color = "red", size = 2, alpha = .7) +
  scale_x_continuous(limits = c(0, 360), 
                     breaks=seq(0,360-1,by=45),
                     labels=c("N","N-E","E","S-E","S","S-W","W","N-W")) +
  coord_polar() +
  theme_minimal(base_size = 14) +
  labs(list(title = "Compass Bearing of DC Bikeshare Rides", x = "", y = "",
            subtitle = paste0(hour, ":00 to ", hour, ":59"),
            caption = paste("Data from www.capitalbikeshare.com
                            Total rides = ", bike_hour_direction$n[hour + 1]))) +
  theme(axis.text.y=element_blank())


##### Add to the above graph a backdrop of DC Map
##### Arrow between start and stop of that hour for every hour
##### Remove the N, N-E, etc labels if I can add the map


## Create 24 plots for GIF of above viz

for (i in c(0:23)) {
  ggplot(filter(bike_time_bear, start.hour == i),
         aes(x = bike.bear)) +
    geom_histogram(bins = 88, alpha = .5) +
    geom_vline(data = filter(bike_hour_direction, start.hour == i),
               aes(xintercept = mean(bike.bear, na.rm = TRUE)),
               color = "red", size = 2, alpha = .7) +
    scale_x_continuous(limits = c(0, 360), 
                       breaks=seq(0,360-1,by=45),
                       labels=c("N","N-E","E","S-E","S","S-W","W","N-W")) +
    coord_polar() +
    theme_minimal(base_size = 14) +
    labs(list(title = "Compass Bearing of DC Bikeshare Rides", x = "", y = "",
              subtitle = paste0(i, ":00 to ", i, ":59"),
              caption = paste("Data from www.capitalbikeshare.com
                              Total rides = ", bike_hour_direction$n[i + 1]))) +
    theme(axis.text.y=element_blank())
  
  ggsave(paste0("images/medium bearing/All GIF/bearing compass", 
                i, ".png"),
         height = 15, width = 15, units = "cm")
  
  print(i)
  
}


### Viz of direction for each station, for each hour

## find a list of station names

bike_stations <- read_csv("~/R/Bikeshare/DC Bikeshare Locations.csv")

bike_stations <- bike_stations %>%
  select(id = ID,
         station = ADDRESS)

top_station <- bike_time %>%
  group_by(start.station) %>%
  summarise(rides = length(start.station)) %>%
  arrange(desc(rides))

#### 14 and V

station <- "14th & V St NW"

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
  
  ggplot(filter(bike_time_bear, 
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


#### Lamont & MtP Viz

station <- "Lamont & Mt Pleasant NW"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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


## 14 and D SE

station <- "14th & D St SE"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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



## Georgia and New Hampshire

station <- "Georgia & New Hampshire Ave NW"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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


### 8th & East Capitol St NE

station <- "8th & East Capitol St NE"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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

#### Maryland & Independence Ave SW


station <- "Maryland & Independence Ave SW"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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


### Columbus Circle / Union Station

station <- "Columbus Circle / Union Station"

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
  ggplot(filter(bike_time_bear, 
                start.hour == i, start.station == station),
         aes(x = bike.bear)) +
    geom_histogram(bins = 45, alpha = .5) +
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
  
  ggsave(paste0("images/medium bearing/", "Union Station", " GIF/bearing compass", 
                i, ".png"),
         height = 15, width = 15, units = "cm")
  
  print(i)
  
}


