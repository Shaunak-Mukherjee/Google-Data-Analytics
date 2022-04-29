#====================================================
# # install required packages 
#====================================================

library(tidyverse) # helps wrangle data
library(lubridate) # helps wrangle date attributes
library(skimr) # helps to provide summary statistics
library(dplyr) # helps with data manipulation
library(ggplot2) #helps visualize data 
library(arsenal) # helps with statistical summaries
library('janitor') # helps with examining and cleaning dirty data

# displays your working directory
getwd() 

# sets your working directory to simplify calls to data
setwd("C:\\....\\") 

#====================================================
# step 1: collect Data 
#====================================================
# Uploaded the Divvy data sets (csv files) here

T_Apr20 <- read_csv('202004-divvy-tripdata.csv')
T_May20 <- read_csv('202005-divvy-tripdata.csv')
T_June20 <- read_csv('202006-divvy-tripdata.csv')
T_July20 <- read_csv('202007-divvy-tripdata.csv')
T_Aug20 <- read_csv('202008-divvy-tripdata.csv')
T_Sep20 <- read_csv('202009-divvy-tripdata.csv')
T_Oct20 <- read_csv('202010-divvy-tripdata.csv')
T_Nov20 <- read_csv('202011-divvy-tripdata.csv')
T_Dec20 <- read_csv('202012-divvy-tripdata.csv')
T_Jan21 <- read_csv('202101-divvy-tripdata.csv')
T_Feb21 <- read_csv('202102-divvy-tripdata.csv')
T_Mar21 <- read_csv('202103-divvy-tripdata.csv')
T_Apr21 <- read_csv('202004-divvy-tripdata.csv')
T_May21 <- read_csv('202105-divvy-tripdata.csv')
T_June21 <- read_csv('202106-divvy-tripdata.csv')
T_July21 <- read_csv('202107-divvy-tripdata.csv')
T_Aug21 <- read_csv('202108-divvy-tripdata.csv')
T_Sept21 <- read_csv('202109-divvy-tripdata.csv')
T_Oct21 <- read_csv('202110-divvy-tripdata.csv')
T_Nov21 <- read_csv('202111-divvy-tripdata.csv')
T_Dec21 <- read_csv('202112-divvy-tripdata.csv')
T_Jan22 <- read_csv('202201-divvy-tripdata.csv')
T_Feb22 <- read_csv('202202-divvy-tripdata.csv')
T_Mar22 <- read_csv('202203-divvy-tripdata.csv')

#====================================================
# step 2: data wrangling and converting into single file 
#====================================================

# first comparing the oldest vs newest data set to check consistency
summary(comparedf(T_Apr20,T_Mar22))

# compare column names each of the files by checking individual month data
colnames(T_Apr20)
colnames(T_May20)
colnames(T_June20)
colnames(T_July20)
colnames(T_Aug20)
colnames(T_Sep20)
colnames(T_Oct20)
colnames(T_Nov20)
colnames(T_Dec20)
colnames(T_Jan21)
colnames(T_Feb21)
colnames(T_Mar21)
colnames(T_Apr21)
colnames(T_May21)
colnames(T_June21)
colnames(T_July21)
colnames(T_Aug21)
colnames(T_Sept21)
colnames(T_Oct21)
colnames(T_Nov21)
colnames(T_Dec21)
colnames(T_Jan22)
colnames(T_Feb22)
colnames(T_Mar22)

# more detailed check to find out data types
str(T_Apr20) 
str(T_May20)
str(T_June20)
str(T_July20)
str(T_Aug20)
str(T_Sep20)
str(T_Oct20)
str(T_Nov20)
str(T_Dec20)
str(T_Jan21)
str(T_Feb21)
str(T_Mar21)
str(T_Apr21)
str(T_May21)
str(T_June21)
str(T_July21)
str(T_Aug21)
str(T_Sept21)
str(T_Oct21)
str(T_Nov21)
str(T_Dec21)
str(T_Jan22)
str(T_Feb22)
str(T_Mar22)


# comparing all data sets from oldes to the newest
compare_df_cols(T_Apr20, T_May20, T_June20, T_July20,
                T_Aug20, T_Sep20, T_Oct20, T_Nov20, T_Dec20,
                T_Jan21, T_Feb21, T_Mar21, T_Apr21, T_May21,
                T_June21, T_July21, T_Aug21, T_Sept21,
                T_Oct21, T_Nov21, T_Dec21, T_Jan22, T_Feb22,
                T_Mar22, return = "mismatch")

 
# We need to convert the data type of end_station_id and start_station_id to 'character' so that they can stack correctly

T_Apr20 <- mutate(T_Apr20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

T_May20 <- mutate(T_May20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

T_June20 <- mutate(T_June20, end_station_id =
                         as.character(end_station_id), start_station_id =
                         as.character(start_station_id))

T_July20 <- mutate(T_July20, end_station_id =
                         as.character(end_station_id), start_station_id =
                         as.character(start_station_id))

T_Aug20 <- mutate(T_Aug20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

T_Sep20 <- mutate(T_Sep20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

T_Oct20 <- mutate(T_Oct20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

T_Nov20 <- mutate(T_Nov20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
  
T_Apr21 <- mutate(T_Apr21, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

## bind individual data frames into one big data frame
all_trips <- bind_rows(T_Apr20, T_May20, T_June20, T_July20,
                       T_Aug20, T_Sep20, T_Oct20, T_Nov20, T_Dec20,
                       T_Jan21, T_Feb21, T_Mar21, T_Apr21, T_May21,
                       T_June21, T_July21, T_Aug21, T_Sept21,
                       T_Oct21, T_Nov21, T_Dec21, T_Jan22, T_Feb22,
                       T_Mar22)

# remove unused column (recommended)
# all_trips <- all_trips %>%
#   select(-c(start_lat, start_lng, end_lat, end_lng))

## rename columns so they can be stacked into one file
all_trips <- all_trips %>% rename(trip_id = ride_id ,ride_type =
                                  rideable_type 
                                  ,start_time = started_at,end_time =ended_at
                                  ,from_station_name = start_station_name
                                  ,from_station_id = start_station_id
                                  ,to_station_name = end_station_name
                                  ,to_station_id = end_station_id
                                  ,usertype = member_casual,
                                  start_long = start_lng,
                                  end_long = end_lng)

#======================================================
# step 3: cleaned up data and prepared for analyis
#======================================================

# inspect the new data table that has been created
# provides a broad overview of a data frame.
colnames(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)
skim(all_trips) 

# add columns that list the date, month, day, and year of each ride. 
# this will allow us to aggregate ride data for each month, day, or year

all_trips$date <- as.Date(all_trips$start_time) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)
typeof(all_trips$ride_length)
is.numeric(all_trips$ride_length)

# convert "ride_length" to numeric
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# try to find if data columns have null or N/A values.
sum(is.na(all_trips$trip_id)) # no null found
sum(is.na(all_trips$ride_type)) # no null found
sum(is.na(all_trips$start_time)) # no null found
sum(is.na(all_trips$end_time)) # no null found
sum(is.na(all_trips$from_station_name)) # has many null values 841495
sum(is.na(all_trips$from_station_id)) # has many null values 8412118
sum(is.na(all_trips$to_station_name)) # has many null values 911414
sum(is.na(all_trips$start_lat)) # no null found
sum(is.na(all_trips$start_long)) # no null found
sum(is.na(all_trips$end_lat))# has many null values 9286
sum(is.na(all_trips$end_long)) # has many null values 9286
sum(is.na(all_trips$usertype)) # no null found

# created a new version (total_trips_v2) of the data set removing the NA values
all_trips_v2<-na.omit(all_trips)
View(all_trips_v2)

# removed "bad" data and create a new data frame
# the dataframe included a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
all_trips_v3 <- all_trips_v2[!(all_trips_v2$from_station_name == "HQ QR" | all_trips_v2$ride_length<0),]
View(all_trips_v3)

#=====================================
# step 4: descriptive data analysis 
#=====================================

# analysis on ride_length (all figures in seconds)
mean(all_trips_v3$ride_length) #straight average (total ride length / rides)
median(all_trips_v3$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v3$ride_length) #longest ride
min(all_trips_v3$ride_length) #shortest ride
summary(all_trips_v3$ride_length) # or could use summary stat

# compared members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype, FUN = min)

# the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype + all_trips_v3$day_of_week, FUN = mean)

# fixing the order of days in the week
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype + all_trips_v3$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v3 %>% 
  mutate(day_of_week = wday(start_time, label = TRUE)) %>%  #creates day_of_week field using wday()
  group_by(usertype, day_of_week) %>%  #groups by usertype and day_of_week
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, day_of_week)								# sorts

#======================================================
# step 5: visualization of data
#======================================================

# visualize the number of rides by rider type
all_trips_v3 %>% 
  mutate(day_of_week = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge") + labs(x = "Day of week", y = "Number of rides",
                                    title ="Number of rides by day of the week") +
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^6)),
                     labels=function(x) x / 100000,
                     limits=c(0,1000000))+labs(fill="User type") + 
                     scale_fill_brewer(palette=c("Dark2"))

# visualization for average duration
all_trips_v3 %>% 
  mutate(day_of_week = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, day_of_week) %>%   summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge") + 
  labs(x = "Day of week", y = "Average trip duration (mins)", 
  title ="Average trip duration by day of week") + scale_y_time()+
  scale_fill_brewer(palette=c("Dark2"))

# visualization  the number of rides by by months 
all_trips_v3 %>% 
  mutate(year_and_month = format(as.Date(all_trips_v3$start_time), "%Y-%m")) %>%
  group_by(usertype, year_and_month) %>% 
  summarise(number_of_rides = n(), .groups = "keep") %>% 
  arrange(usertype, year_and_month)  %>% 
  ggplot(aes(x = year_and_month, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")+
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,400000))+
  labs(x = "Year-Month", y = "Number of rides",
       title = "Number of rides by month",
       fill = "User type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+ 
  scale_fill_brewer(palette=c("Dark2"))


# visualization for average duration by bike type
all_trips_v3 %>%
  mutate(year_and_month = format(as.Date(all_trips_v3$start_time), "%Y-%m")) %>%
  group_by(year_and_month, ride_type) %>% 
  summarise(average_duration = mean(ride_length), .groups = "keep") %>% 
  arrange(year_and_month, ride_type)  %>% 
  ggplot(aes(x = year_and_month, y = average_duration, fill = ride_type)) +
  geom_col(position = "dodge")+
  labs(x = "Year-Month", y = "Duration in min",
       title = "Average duration by bike type",
       fill = "Bike type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_brewer(palette=c("Dark2"))

# visualization for cyclists preferences over bike type
all_trips_v3 %>%
  group_by(usertype, ride_type) %>% 
  summarise(number_of_rides = n(), .groups = "keep") %>% 
  arrange(usertype, ride_type)  %>% 
  ggplot(aes(x = usertype, y = number_of_rides, fill = ride_type)) +
  geom_col(position = "dodge")+
  labs(x = "Customer type", y = "Number of rides",
       title = "Customer preferences over bike type",
       fill = "Bike type")+scale_fill_brewer(palette=c("Dark2"))

# visualization for number of rides on working days
all_trips_v3 %>%
  filter(wday(start_time) != 1 | wday(start_time) != 7) %>%
  mutate(hour = hour(all_trips_v3$start_time)) %>%
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n(), .groups = "keep") %>% 
  arrange(hour , number_of_rides)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")+
  scale_y_continuous(name=expression(Number ~ of ~ Rides ~ (x10^5)),
                     labels=function(x) x / 100000,
                     limits=c(0,500000))+
  scale_x_continuous(breaks = 0:23)+
  labs(x = "Hour", y = "Number of rides",
       title = "Number of rides on workdays",
       fill = "User type")+
  scale_fill_brewer(palette=c("Dark2"))


# visualization for number of rides on weekends
all_trips_v3 %>%
  mutate(hour = hour(all_trips_v3$start_time)) %>%
  filter(wday(start_time) == 7 | wday(start_time) == 1) %>%
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n(), .groups = "keep") %>% 
  arrange(hour , number_of_rides)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")+
  scale_x_continuous(breaks = 0:23)+
  labs(x = "Hour", y = "Number of rides",
       title = "Number of rides on weekends",
       fill = "User type")+
  scale_fill_brewer(palette=c("Dark2"))

# final count of rides per week by different riders
counts <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$usertype + 
                      all_trips_v3$day_of_week, FUN = mean)

#=================================================
# step 6: export summary file for further analysis
#=================================================

#Export to CSV file for further analysis
write.csv(all_trips_v2, "Final_data.csv")
