library(tidyverse)
library(lubridate)
library(ggplot2) 


tripdata_202106 <- read.csv("202106-divvy-tripdata.csv")
tripdata_202107 <- read.csv("202107-divvy-tripdata.csv")
tripdata_202108 <- read.csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("202112-divvy-tripdata.csv")
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("202203-divvy-tripdata.csv")
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")


colnames(tripdata_202106)
colnames(tripdata_202107)
colnames(tripdata_202108)
colnames(tripdata_202109)
colnames(tripdata_202110)
colnames(tripdata_202111)
colnames(tripdata_202112)
colnames(tripdata_202201)
colnames(tripdata_202202)
colnames(tripdata_202203)
colnames(tripdata_202204)
colnames(tripdata_202205)


str(tripdata_202106)
str(tripdata_202107)
str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)
str(tripdata_202201)
str(tripdata_202202)
str(tripdata_202203)
str(tripdata_202204)
str(tripdata_202205)


combined_data <- bind_rows(tripdata_202106, tripdata_202107, tripdata_202108, tripdata_202109,
                           tripdata_202110, tripdata_202111, tripdata_202112, tripdata_202201,
                           tripdata_202202, tripdata_202203, tripdata_202204, tripdata_202205)
colnames(combined_data)
str(combined_data)

write.csv(combined_data, "data.csv", row.names=FALSE)


combined_data[['started_at']] <- ymd_hms(combined_data[['started_at']])
combined_data[['ended_at']] <- ymd_hms(combined_data[['ended_at']])
str(combined_data)


combined_data <- combined_data %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
glimpse(combined_data)


combined_data <- combined_data %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(combined_data)


dim(combined_data)
summary(combined_data)
length(unique(combined_data$customer_type))


combined_data$date <- as.Date(combined_data$start_time)
combined_data$month <- format(as.Date(combined_data$date), "%m")
combined_data$day <- format(as.Date(combined_data$date), "%d")
combined_data$year <- format(as.Date(combined_data$date), "%Y")
combined_data$day_of_week <- format(as.Date(combined_data$date), "%A")


combined_data$ride_length <- as.numeric(difftime(combined_data$end_time,combined_data$start_time))
glimpse(combined_data)
head(combined_data)


nrow(combined_data[combined_data$start_station_name == "", ])

combined_data_clean <- combined_data[!(combined_data$start_station_name == "" | combined_data$ride_length<0),]
dim(combined_data_clean)
glimpse(combined_data_clean)
head(combined_data_clean)


summary(combined_data_clean$ride_length)
summarise(combined_data_clean, mean_rd = mean(ride_length), min_re = min(ride_length),
          median_rd = median(ride_length), max_rd = max(ride_length))


aggregate(combined_data_clean$ride_length ~ combined_data_clean$customer_type, FUN = mean)
aggregate(combined_data_clean$ride_length ~ combined_data_clean$customer_type, FUN = median)
aggregate(combined_data_clean$ride_length ~ combined_data_clean$customer_type, FUN = max)
aggregate(combined_data_clean$ride_length ~ combined_data_clean$customer_type, FUN = min)


aggregate(combined_data_clean$ride_length ~ combined_data_clean$customer_type + combined_data_clean$day_of_week, FUN = mean)
combined_data_clean$day_of_week <- ordered(combined_data_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
combined_data_clean$month <- ordered(combined_data_clean$month, levels=c("06", "07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05"))


combined_data_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  
  group_by(customer_type, weekday) %>%  
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)
  
  
combined_data_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge")
  

combined_data_clean %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge")


combined_data_clean %>% 
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge")


combined_data_clean %>% 
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge")


combined_data_clean %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")
