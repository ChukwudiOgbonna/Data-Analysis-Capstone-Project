# Install and load libraries
install.packages("tidyverse")
install.packaged("lubridate")
library(tidyverse)
library(lubridate)
# set wd as your file location
setwd("/Users/chuks/Downloads/Cycle_data_past_12_months")

# load all data for the past 12 months
august_2021 <- read_csv("August_2021/202108-divvy-tripdata.csv")
july_2021 <- read_csv("July_2021/202107-divvy-tripdata.csv")
june_2021 <- read_csv("June_2021/202106-divvy-tripdata.csv")
may_2021 <- read_csv("May_2021/202105-divvy-tripdata.csv")
april_2022 <- read_csv("April_2022/202204-divvy-tripdata.csv")
march_2022 <- read_csv("March_2022/202203-divvy-tripdata.csv")
february_2022 <- read_csv("February_2022/202202-divvy-tripdata.csv")
january_2022 <- read_csv("January_2022/202201-divvy-tripdata.csv")
december_2021 <- read_csv("December_2021/202112-divvy-tripdata.csv")
november_2021 <- read_csv("November_2021/202111-divvy-tripdata.csv")
october_2021 <- read_csv("October_2021/202110-divvy-tripdata.csv")
september_2021 <- read_csv("September_2021/202109-divvy-tripdata.csv")
# Rename columns to make them consistent
 april_2022 <- rename(april_2022,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)
 march_2022 <- rename(march_2022,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

february_2022 <- rename(february_2022,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

january_2022 <- rename(january_2022,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

december_2021 <- rename(december_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

november_2021 <- rename(november_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

october_2021 <- rename(october_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

september_2021 <- rename(september_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

august_2021 <- rename(august_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

july_2021<- rename(july_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

june_2021 <- rename(june_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

may_2021 <- rename(may_2021,ride_id=trip_id,rideable_type=bikeid,started_at=start_time,ended_at=end_time,start_station_name=from_station_name,start_station_id=from_station_id,end_station_name=to_station_name,end_station_id=to_station_id,member_casual=usertype)

 

    # combine all our data frames into one data frame
    all_trips <- bind_rows(april_2022,march_2022,february_2022,january_2022,december_2021,november_2021,october_2021,september_2021,august_2021,july_2021,june_2021,may_2021)
    # Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
  all_trips <- all_trips %>% select(-c(start_lat,start_lng,end_lat,end_lng))
  # summary statistics for the new table
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
# aggregating the data using rider type is too granular, let us create new columns for date month and year, and group our data using that
# Extract the Date, day month and year
all_trips$date <- as.Date(all_trips$started_at)
 all_trips<- mutate(all_trips, day= format(all_trips$date,'%d'))
 all_trips<- mutate(all_trips, month= format(all_trips$date,'%m'))
 all_trips<- mutate(all_trips, year= format(all_trips$date,'%Y'))
 
 
 # add column ride length as end time- start time
  all_trips<- mutate(all_trips,ride_length=ended_at-started_at)
        
    # add weekday column
    all_trips<- mutate(all_trips, day_of_week=format(as.Date(all_trips$date), "%A") )
    # convert our duration ride_length to numeric value( double)
    all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
    # Remove bad data, remove data that start snation name is HQ QR and the ride length is negative
    all_trips_v2 <- filter(all_trips, ride_length>0 | start_station_name!= "HQ QR")
# conduct analysis stage
# descriptive analysis on ride_length
summary(all_trips_v2$ride_length)
# aggregate our data based on the member_causal column and find summary statistics
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
# the ordered function makes sure that when data is aggregated, it will follow the order in the vector below
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# group by member type and day and find the mean ride length
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% mutate(weekday= wday(started_at,label= TRUE)) %>% group_by(member_casual,weekday) %>% summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% arrange(member_casual,weekday)ubric   ## create weekday column using wday funtion then group by member causal and weekday, then find the number of rides in each group and the mean ride length in each group then arrange/sort our results by member type and weekday

# TIME TO VISUALIZE OUR ANALYSIS
all_trips_v2 %>% mutate(weekday= wday(started_at,label= TRUE)) %>% group_by(member_casual,weekday) %>% summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% arrange(member_casual,weekday) %>% ggplot()+ geom_col(mapping= aes(x= weekday,y=average_duration, fill=member_casual,position="dodge"))

ggsave("visualization.png")
# let us write summary statistsics to csv file
summary_stats <- all_trips_v2 %>% group_by(member_casual,day_of_week)%>% summarise(average_length_ride=mean(ride_length))%>% arrange(day_of_week,member_casual)

write.csv(summary_stats, file = 'avg_ride_length.csv')
