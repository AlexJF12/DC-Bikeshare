## Capital Bikeshare
## Heatmap
## By Alex Freeman

library(tidyverse)
library(lubridate)
library(viridis)

## Set working directory, remove hash

#setwd("~/R/Bikeshare")

## Data created by 'dc bikeshare munging.R' file

bike <- read_rds("~/R/Bikeshare/bike_full.rds")

## Create heatmap data.table

bike_day_heatmap <- bike %>%
  select(start.date) %>%
  group_by(start.date) %>%
  summarise(n = n())

bike_day_heatmap$days <- factor(weekdays(bike_day_heatmap$start.date,T), 
                            levels = rev(c("Mon", "Tue", "Wed", 
                                           "Thu","Fri", "Sat", "Sun")))
bike_day_heatmap$week<-as.numeric(format(bike_day_heatmap$start.date,"%W"))
bike_day_heatmap$month<-as.numeric(format(bike_day_heatmap$start.date,"%m"))
bike_day_heatmap$year<-as.numeric(format(bike_day_heatmap$start.date,"%Y"))

## Heatmapviz day by day

ggplot(bike_day_heatmap, aes(x = week, y = days, fill = n)) +
  scale_fill_viridis(name="# of Rides", option = "C", 
                     limits = c(0, max(bike_day_heatmap$n))) + 
  geom_tile(color = "white", size = 0.4) + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme_minimal() + 
  theme(legend.position = "bottom") +
  labs(title = "DC Bikeshare: Heatmap of Rides taken per Day",
       x = "Month", y = "Day of the Week",
       subtitle = "July 1, 2015 to June 30, 2016",
       caption = "Data from: https://s3.amazonaws.com/capitalbikeshare-data/index.html")

ggsave("images/bike daily heatmap.png", width = 10, height = 4)

## Create bike time heatmap data

bike_time <- bike %>%
  select(start.date, start.time)

bike_time$start.hour <- hour(bike_time$start.time)

bike_time_heatmap <- bike_time %>%
  group_by(start.date, start.hour) %>%
  summarise(n = n())

bike_time_heatmap$days <- factor(weekdays(bike_time_heatmap$start.date,T), 
                            levels = (c("Mon", "Tue", "Wed", 
                                           "Thu","Fri", "Sat", "Sun")))
bike_time_heatmap$year<-as.numeric(format(bike_time_heatmap$start.date,"%Y"))

## Heatmap time viz

ggplot(bike_time_heatmap, aes(x = days, y = start.hour, fill = n)) +
  scale_fill_viridis(name="# of Rides", option = "C", 
                     limits = c(0, max(bike_time_heatmap$n))) +
  geom_tile(color = "white", size = 0.4) +
  theme_minimal()  + 
  scale_y_reverse() +
  labs(title = "DC Bikeshare: Heatmap of Rides taken per Hour",
       x = "Day of the Week", y = "Starting Hour",
       subtitle = "July 1, 2015 to June 30, 2016",
       caption = "Data from: https://s3.amazonaws.com/capitalbikeshare-data/index.html")

ggsave("images/bike time heatmap.png", width = 6, height = 9)


## Heatmap time viz, by account type

# Make data

bike_time_account <- bike %>%
  select(start.date, start.time, account)

bike_time_account$start.hour <- hour(bike_time_account$start.time)

bike_time_heatmap_account <- bike_time_account %>%
  group_by(start.date, start.hour, account) %>%
  summarise(n = n())

bike_time_heatmap_account$days <- factor(weekdays(bike_time_heatmap_account$start.date,T), 
                                 levels = (c("Mon", "Tue", "Wed", 
                                             "Thu","Fri", "Sat", "Sun")))
bike_time_heatmap_account$year <- as.numeric(
  format(bike_time_heatmap_account$start.date,"%Y"))


# Plot

ggplot(bike_time_heatmap_account, aes(x = days, y = start.hour, fill = n)) +
  scale_fill_viridis(name="# of Rides", option = "C", 
                     limits = c(0, max(bike_time_heatmap$n))) +
  geom_tile(color = "white", size = 0.4) +
  theme_minimal()  + 
  scale_y_reverse() +
  facet_wrap("bike_time_heatmap_account$account") +
  labs(title = "DC Bikeshare: Heatmap of Rides taken per Hour",
       x = "Day of the Week", y = "Starting Hour",
       subtitle = "Casual and Registered Riders. July 1, 2015 to June 30, 2016",
       caption = "Data from: https://s3.amazonaws.com/capitalbikeshare-data/index.html")

ggsave("images/bike time heatmap account.png", width = 6, height = 6)


### Analysis

table(bike$account)
summary(bike_day_heatmap)

library(data.table)
bike_day_heatmap <- as.data.table(bike_day_heatmap)
bike_day_heatmap[, sum(n), by = days]
