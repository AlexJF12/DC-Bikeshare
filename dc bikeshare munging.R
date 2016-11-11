## Capital Bikeshare
## Collect, Clean and Slice Data
## By Alex Freeman

## Data downloaded from:
## https://s3.amazonaws.com/capitalbikeshare-data/index.html

################################################################

setwd("~/R/Bikeshare")

library(tidyverse)

# Load and combine 4 quarters of data

bike2015Q3 <- read.csv("~/R/Bikeshare/2015-Q3-cabi-trip-history-data.csv")
bike2015Q4 <- read.csv("~/R/Bikeshare/2015-Q4-Trips-History-Data.csv")
bike2016Q1 <- read.csv("~/R/Bikeshare/2016-Q1-Trips-History-Data.csv")
bike2016Q2 <- read.csv("~/R/Bikeshare/2016-Q2-Trips-History-Data.csv")

names <- names(bike2016Q2)

colnames(bike2016Q1) <- names
colnames(bike2015Q3) <- names
colnames(bike2015Q4) <- names

bike2016Q2$quarter <- 2
bike2016Q1$quarter <- 1
bike2015Q4$quarter <- 4
bike2015Q3$quarter <- 3

names(bike2016Q1) == names(bike2016Q2)
names(bike2015Q4) == names(bike2015Q3)

bike <- rbind(bike2015Q3, bike2015Q4)
bike <- rbind(bike, bike2016Q1)
bike <- rbind(bike, bike2016Q2)

rm(bike2016Q2)
rm(bike2016Q1)
rm(bike2015Q4)
rm(bike2015Q3)
rm(names)


# Rename variables, reclass variables, create variables

colnames(bike) <-c("duration", "start.date.time", "end.date.time", 
                   "start.station.number", "start.station",
                   "end.station.number", "end.station",
                   "bike.number", "account", "quarter")

bike$quarter <- as.factor(bike$quarter)

bike$start.date <- as.Date(bike$start.date.time, "%m/%d/%Y %H:%M")
bike$start.time <- strptime(bike$start.date.time, "%m/%d/%Y %H:%M")

bike$end.date <- as.Date(bike$end.date.time, "%m/%d/%Y %H:%M")
bike$end.time <- strptime(bike$end.date.time, "%m/%d/%Y %H:%M")
bike$day <- factor(weekdays(bike$start.date), 
                   levels= c("Sunday", "Monday", 
                             "Tuesday", "Wednesday", 
                             "Thursday", "Friday", 
                             "Saturday"))

  ##Convert duration to minutes
bike$duration <- as.numeric(bike$duration)
bike$duration <- bike$duration / 60000

write.csv(bike, "bikeshare 2015Q3 to 2016Q2.csv")
saveRDS(bike, "bike_full.rds")

## Make small datasets

set.seed(47)

bike_300k <- bike[sample(1:nrow(bike), 300000,
                           replace = FALSE),]
write.csv(bike_300k, "bike_300k.csv")
saveRDS(bike_300k, "bike_300k.rds")

bike_100k <- bike[sample(1:nrow(bike), 100000,
                          replace = FALSE),]
write.csv(bike_100k, "bike_100k.csv")
saveRDS(bike_100k, "bike_100k.rds")

bike_10k <- bike[sample(1:nrow(bike), 10000,
                           replace = FALSE),]
write.csv(bike_10k, "bike_10k.csv")
saveRDS(bike_10k, "bike_10k.rds")

bike_1k <- bike[sample(1:nrow(bike), 1000,
                        replace = FALSE),]
write.csv(bike_1k, "bike_1k.csv")
saveRDS(bike_1k, "bike_1k.rds")