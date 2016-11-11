## Capital Bikeshare
## Early Viz Data
## By Alex Freeman

######################################################

library(tidyverse)

# Read in data from `dc bikeshare munging.R`

bike_full <- read_rds("~/R/Bikeshare/bike_full.rds")
bike_300k <- read_rds("~/R/Bikeshare/bike_300k.rds")
bike_100k <- read_rds("~/R/Bikeshare/bike_100k.rds")
bike_10k <- read_rds("~/R/Bikeshare/bike_10k.rds")
bike_1k <- read_rds("~/R/Bikeshare/bike_1k.rds")

# Easy Analysis

summary(bike_full$duration)
summary(bike_300k$duration)
summary(bike_100k$duration)
summary(bike_10k$duration)
summary(bike_1k$duration)

## Distribution of Duration of trip by different n, seperate

ggplot(subset(bike_1k, duration < 60), aes(duration, ..density..)) +
  geom_histogram(bins = 120, fill = "darkblue") +
  labs(title = "Duration of trip, n = 1,000",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration 1k.png")

ggplot(subset(bike_10k, duration < 60), aes(duration, ..density..)) +
  geom_histogram(bins = 120, fill = "darkred") +
  labs(title = "Duration of trip, n = 10,000",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration 10k.png")

ggplot(subset(bike_100k, duration < 60), aes(duration, ..density..)) +
  geom_histogram(bins = 120, fill = "darkorange") +
  labs(title = "Duration of trip, n = 100,000",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration 100k.png")

ggplot(subset(bike_300k, duration < 60), aes(duration, ..density..)) +
  geom_histogram(bins = 120, fill = "darkgreen") +
  labs(title = "Duration of trip, n = 300,000",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration 300k.png")

ggplot(subset(bike_full, duration < 60), aes(duration, ..density..)) +
  geom_histogram(bins = 120, fill = "darkmagenta") +
  labs(title = "Duration of trip, n = 3,257,101",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration full.png")

## Distribution of Duration of trip by different n, combined

ggplot() +
  geom_histogram(data = subset(bike_1k, duration < 60), 
                 aes(duration, ..density..),
                 bins = 120, fill = "darkblue", alpha = .25) +
  geom_histogram(data = subset(bike_10k, duration < 60),
                 aes(duration, ..density..), 
                 bins = 120, fill = "darkred", alpha = .25) +
  geom_histogram(data = subset(bike_100k, duration < 60),
                 aes(duration, ..density..), 
                 bins = 120, fill = "darkorange", alpha = .25) +
  geom_histogram(data = subset(bike_300k, duration < 60),
                 aes(duration, ..density..), 
                 bins = 120, fill = "darkgreen", alpha = .25) +
  geom_histogram(data = subset(bike_full, duration < 60),
                 aes(duration, ..density..), 
                 bins = 120, fill = "darkmagenta", alpha = .25) +
  labs(title = "Duration of trip, n = 1k, 10k, 100k, 300k, 3,2m",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration combined.png")

## Distribution of Duration of trip by different n, 1k and 3.2m


ggplot() +
  geom_histogram(data = subset(bike_1k, duration < 60), 
                 aes(duration, ..density..),
                 bins = 120, fill = "darkblue", alpha = .25) +
  geom_histogram(data = subset(bike_full, duration < 60),
                 aes(duration, ..density..), 
                 bins = 120, fill = "darkmagenta", alpha = .25) +
  labs(title = "Duration of trip, n = 1k, 3.2m",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration 1k 3.2m combined.png")

## Denisty distribution of Duration of trip by different n, combined

ggplot() +
  geom_density(data = subset(bike_1k, duration < 60), 
                 aes(duration, ..density..),
                 fill = "darkblue", alpha = .25) +
  geom_density(data = subset(bike_full, duration < 60),
                 aes(duration, ..density..), 
                 fill = "darkmagenta", alpha = .25) +
  labs(title = "Duration of trip, n = 1k and 3.2m",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration density combined.png")


## Duration by Day of Week

ggplot(subset(bike_full, duration < 60), aes(duration)) + 
  geom_histogram(aes(fill = day), bins = 60, position = "fill") +
  labs(title = "Duration of trip",
       x = "Duration (mins)",
       y = "Proportion of Trips") +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("images/bike duration day of week.png")

ggplot(subset(bike_full, duration < 60), aes(duration)) + 
  geom_histogram(aes(fill = day), bins = 60, position = "fill") +
  facet_wrap(~account, nrow = 2) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Duration of trip by Account Type",
       x = "Duration (mins)",
       y = "Proportion of Trips")
ggsave("images/bike duration day account type.png")

