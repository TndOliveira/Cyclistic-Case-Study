

## PREPARE



# Setting the environment



# installing packages tidyverse, janitor, skimr, lubridate e ggplot
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("janitor", repos = "http://cran.us.r-project.org")
install.packages("skimr", repos = "http://cran.us.r-project.org")
install.packages("lubridate", repos = "http://cran.us.r-project.org")
install.packages("ggplot", repos = "http://cran.us.r-project.org")


# loading packages tidyverse, janitor, skimr, lubridate e ggplot2
library("tidyverse")
library("janitor")
library("skimr")
library("lubridate")
library("ggplot2")


# importing datasets
tripdata_2022_03 <- read.csv("2022-03_tripdata.csv")
tripdata_2022_04 <- read.csv("2022-04_tripdata.csv")
tripdata_2022_05 <- read.csv("2022-05_tripdata.csv")
tripdata_2022_06 <- read.csv("2022-06_tripdata.csv")
tripdata_2022_07 <- read.csv("2022-07_tripdata.csv")
tripdata_2022_08 <- read.csv("2022-08_tripdata.csv")
tripdata_2022_09 <- read.csv("2022-09_tripdata.csv")
tripdata_2022_10 <- read.csv("2022-10_tripdata.csv")
tripdata_2022_11 <- read.csv("2022-11_tripdata.csv")
tripdata_2022_12 <- read.csv("2022-12_tripdata.csv")
tripdata_2023_01 <- read.csv("2023-01_tripdata.csv")
tripdata_2023_02 <- read.csv("2023-02_tripdata.csv")




## PROCESS




# checking the structure
str(tripdata_2022_03)
str(tripdata_2022_04)
str(tripdata_2022_05)
str(tripdata_2022_06)
str(tripdata_2022_07)
str(tripdata_2022_08)
str(tripdata_2022_09)
str(tripdata_2022_10)
str(tripdata_2022_11)
str(tripdata_2022_12)
str(tripdata_2023_01)
str(tripdata_2023_02)


# checking the columns matching
compare_df_cols(tripdata_2022_03, tripdata_2022_04, tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08, tripdata_2022_09, tripdata_2022_10, tripdata_2022_11, tripdata_2022_12, tripdata_2023_01, tripdata_2023_02)


# merging data
tripdata <- bind_rows(tripdata_2022_03, tripdata_2022_04, tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08, tripdata_2022_09, tripdata_2022_10, tripdata_2022_11, tripdata_2022_12, tripdata_2023_01, tripdata_2023_02)


# Before I create the "ride_length" column to register the length of the trips, I will filter the dataset to confirm that values from started_at are less than values in ended_at.
tripdata %>%
  filter(started_at > ended_at) %>%
  summarise(rideable_type, started_at, ended_at)


#Filtering the dataset, we found 101 records where the values from started_at are higher than values from ended_at. However, we can conclude that this is not the right way to represent the time of the trips.

#With that in mind, I will create a new dataset only to get these records to analyze them.


# creating a new dataset incorrect_time
incorrect_time <- tripdata %>%
  filter(started_at > ended_at)


# analyzing dataset incorrect_time
incorrect_time %>%
  filter(started_at > ended_at) %>%
  group_by(rideable_type) %>%
  summarise(percent_of_rides = round(length(rideable_type) / nrow(incorrect_time) * 100, digits = 2))


# Of 101 records, 73, or 72.3%, were from electric bikes, and 28, or 27.7%, were from classic bikes.


# analyzing dataset incorrect_time
incorrect_time %>%
  filter(started_at > ended_at) %>%
  group_by(start_station_name, rideable_type) %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  print()


# Here, we observe that from 73 electric bikes in that dataset, 24 were without stations' names, and 11 occurred in the same station, "Lincoln Ave & Roscoe St"; While for 28 classic bikes, 
# we also have 12 occurrences in "Lincoln Ave & Roscoe St"; But that whole record for classic bikes we have station's name.


# Remember that our business task is to identify how casual users and annual members use Cyclistic's bikes differently and not to identify how occurred the incorrect input records in trips' time.
# Noting that this analysis is not our business task, to avoid distractions, I will consider that the error occurred due to software or research imputation errors.

# We can consider this as a future analysis because if we continue to get these errors, it can impact the accuracy of travel behavior models and lead to biases in the measures derived from the models.


# Considering that it was an error caused by software or research imputation, to solve this problem, I will swap the values between started_at and ended_at so that I correctly have the trip times.


# swaping values 
for (i in 1:nrow(tripdata)) {
  if(tripdata[i,3] > tripdata[i,4]) {
    x <- tripdata[i,3]
    tripdata[i,3] <- tripdata[i,4]
    tripdata[i,4] <- x
  }
}


# After performing the data transformation, I will filter the dataset again to verify that I have the correct time values.


# filtering tripdata
tripdata %>%
  filter(started_at > ended_at)


# Now, there are no more incorrect values in started_at and ended_at. After verifying the dataset, I will create the ride_length column.


# Now, I will order the data through the variable (started_at), then I will create a column called (date) to record the trip date, and I will add other ones called (month, day, year, day_of_week, ride_length, and start_hour).


# Note: The trip duration will be present in seconds.


# sorting column started_at
tripdata <- tripdata %>%
  arrange(started_at)


# creating column date
tripdata <- tripdata %>%
  mutate(date = format(as.Date(started_at)))


# creating new columns
tripdata <- tripdata %>%
  mutate(month = format(as.Date(date), "%B")) %>%
  mutate(day = format(as.Date(date), "%d")) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  mutate(day_of_week = format(as.Date(date), "%A")) %>%
  mutate(ride_length = difftime(ended_at, started_at)) %>%
  mutate(start_hour = strftime(started_at, "%H"))


# The values in month and day_of_week are in Portuguese. So I will translate them into English.

# translating day_of_week
tripdata$day_of_week[tripdata$day_of_week=="domingo"] <- "Sunday"
tripdata$day_of_week[tripdata$day_of_week=="segunda-feira"] <- "Monday"
tripdata$day_of_week[tripdata$day_of_week=="terça-feira"] <- "Tuesday"
tripdata$day_of_week[tripdata$day_of_week=="quarta-feira"] <- "Wednesday"
tripdata$day_of_week[tripdata$day_of_week=="quinta-feira"] <- "Thursday"
tripdata$day_of_week[tripdata$day_of_week=="sexta-feira"] <- "Friday"
tripdata$day_of_week[tripdata$day_of_week=="sábado"] <- "Saturday"


# translating months
tripdata$month[tripdata$month=="janeiro"] <- "January"
tripdata$month[tripdata$month=="fevereiro"] <- "February"
tripdata$month[tripdata$month=="março"] <- "March"
tripdata$month[tripdata$month=="abril"] <- "April"
tripdata$month[tripdata$month=="maio"] <- "May"
tripdata$month[tripdata$month=="junho"] <- "June"
tripdata$month[tripdata$month=="julho"] <- "July"
tripdata$month[tripdata$month=="agosto"] <- "August"
tripdata$month[tripdata$month=="setembro"] <- "September"
tripdata$month[tripdata$month=="outubro"] <- "October"
tripdata$month[tripdata$month=="novembro"] <- "November"
tripdata$month[tripdata$month=="dezembro"] <- "December"



# converting ride_length column data type
tripdata$ride_length <- as.numeric(as.character(tripdata$ride_length))


# checking dataset structure
str(tripdata)


# checking dataset statistic summary
skim_without_charts(tripdata)


# The summary informed us that the dataset contains empty data, missing data and that there is no duplicated data!


# The summary also shows that the ride_length column contains a minimum of 0, which does not make sense for a ride duration of 0 seconds. So, in this analysis, I will consider rows just with trip duration starting from 60 seconds.



# Before we perform any data cleaning process, I will create a dataset backup to ensure we have an original one.


# making the dataset backup
tripdata_v2 <- tripdata


# removing values below 60 seconds from ride_length column
tripdata_v2 <- tripdata_v2[tripdata_v2$ride_length >= 60,]


# Using the summary again, we can see that the minimum value from the "ride_length" column is now 60.


# verifying ride_length column
summary(tripdata_v2$ride_length)


# remove irrelevant data


# For this analysis, I considered the following variables irrelevant. So, I will remove them.


# removing columns
tripdata <- tripdata %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))



# The statistic summary informed us that the dataset contains empty data. Therefore, I will convert them into NAs to ease the data-cleaning process.


# converting empty data to missing data (NAs)
tripdata_v2[tripdata_v2 == ''] <- NA


# After the data transformation, I will remove all missing data.


# removing empty data (NAs)
tripdata_v2 <- na.omit(tripdata_v2)


# Checking the dataset once again


# checking the structure
str(tripdata_v2)


# checking the statistic summary
skim_without_charts(tripdata_v2)


# Once the data are consistent and complete, it is ready to be analyzed.





## Analyze



# descriptive analysis in ride_length column
summary(tripdata_v2$ride_length)


# Comparing casual users and annual members


# aggregation columns
tripdata_v2 %>%
  aggregate(ride_length ~ member_casual, FUN = mean)

tripdata_v2 %>%
  aggregate(ride_length ~ member_casual, FUN = median)

tripdata_v2 %>%
  aggregate(ride_length ~ member_casual, FUN = max)

tripdata_v2 %>%
  aggregate(ride_length ~ member_casual, FUN = min)


# analyzing user type
tripdata_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n(), percent_of_rides = round(length(ride_id) / nrow(tripdata_v2), digits = 4) * 100)


# From 4.416.370 observations, we have 2.651.406 annual members, which represents 60.00%, and 1.764.964 casual users, which represents 40.00%, meaning that we have more annual members using Cyclistic shared bikes.


# analyzing the number of rides by month
tripdata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), percent_of_rides = round(length(ride_id) / nrow(tripdata_v2), digits = 4) * 100) %>%
  print(n = 24)


# The month column is out of order. So, I will sort them.


# sorting the column month
tripdata_v2$month <- ordered(tripdata_v2$month, levels=c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"))


# I will also sort the column day_of_week, that is out of order.


# sorting the column day_of_week
tripdata_v2$day_of_week <- ordered(tripdata_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Now with the columns sorted, we can analyze the summary.


# analyzing the number of rides by month
tripdata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), percent_of_rides = round(length(ride_id) / nrow(tripdata_v2), digits = 4) * 100) %>%
  print(n = 24)



# analyzing the number of rides by weekday
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), percent_of_rides = round(length(ride_id) / nrow(tripdata_v2), digits = 4) * 100)



# analyzing the number of rides by hour
tripdata_v2 %>%
  group_by(start_hour) %>%
  summarise(number_of_rides = n(),
            member = sum(member_casual == "member"),
            casual = sum(member_casual == "casual"),
            difference = abs(sum(member_casual == "member") - sum(member_casual == "casual"))) %>%
  print(n = 24)



# analyzing the average trip duration by month
tripdata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = round(mean(ride_length))) %>%
  print(n = 24)


# analyzing the average trip duration by weekday
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = round(mean(ride_length)))


# analyzing the number of rides by bicycle type
tripdata_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), percent_of_ride = round(length(ride_id) / nrow(tripdata_v2) * 100, digits = 2))


# analyzing the average trip duration by bicycle type
tripdata_v2 %>%
  aggregate(ride_length ~ member_casual + rideable_type, FUN = mean)


# analyzing stations casual
tripdata_v2 %>%
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "casual") %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  head(10)


# analyzing stations members
tripdata_v2 %>%
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "member") %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  head(10)





## SHARE




# removing scientific notations
options(scipen = 999)


# Data visualizations


# visualizing user type
tripdata_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
    geom_col(position = 'dodge') +
    labs(title="Total number of rides by user type",
    caption = "Source: Motivate International Inc.",
    x = "Member vs Casual", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing month
tripdata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "Number of rides by month",
    caption = "Source: Motivate International Inc.", 
    x = "Month", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing day_of_week
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
    ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Number of rides by weekday",
    caption = "Source: Motivate International Inc.", 
    x = "Weekday", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing hour
tripdata_v2 %>%
  group_by(member_casual, start_hour) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = start_hour, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Number of rides by start hour",
    caption = "Source: Motivate International Inc.",
    x = "Hour", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing average month
tripdata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
    geom_col(position = "dodge") +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "Average ride length by month",
    caption = "Source: Motivate International Inc.", 
    x = "Month", y = "Average Ride Length (sec)", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))

# visualizing average day_of_week
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average ride length by weekday",
    caption = "Source: Motivate International Inc.", 
    x = "Weekday", y = "Average Ride Length (sec)", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing bicycle type
tripdata_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  ggplot(aes(x=member_casual, y=number_of_rides, fill=member_casual)) +
    geom_col(position = "dodge") +
    facet_wrap(~rideable_type) +
    labs(title = "Number of rides by bicycle type",
    caption = "Source: Motivate International Inc.",
    x = "Member Casual", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing average bicycle
tripdata_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = rideable_type, y = average_ride_length, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average ride length by bicycle type",
    caption = "Source: Motivate International Inc.",
    x = "Bicycle type", y = "Average Ride Length (sec)", fill = "User Type") +
    scale_fill_manual(values = c("casual" = "#5DADE2", "member" = "#21618C"))


# visualizing stations casual
tripdata_v2 %>%
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "casual") %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  head(10) %>%
  ggplot(aes(x= reorder(start_station_name, number_of_rides), y=number_of_rides, fill=member_casual)) +
    scale_y_continuous(breaks = seq(0, 60000, 9000)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "10 most used stations by casual",
    caption = "Source: Motivate International Inc.",
    x = "Station Name", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("casual" = "#5DADE2"))


# visualizing stations members
tripdata_v2 %>%
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "member") %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  head(10) %>%
  ggplot(aes(x= reorder(start_station_name, number_of_rides), y=number_of_rides, fill=member_casual)) +
    scale_y_continuous(breaks = seq(0, 25000, 5000)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "10 most used stations by member",
    caption = "Source: Motivate International Inc.",
    x = "Station Name", y = "Number of Rides", fill = "User Type") +
    scale_fill_manual(values = c ("member" = "#21618C"))



# I will export my cleaned dataset tripdata_v2, intending to load it in Tableau to visualize our data and create a dashboard.


# exporting the clean dataset
write.csv(tripdata_v2, file = 'path/tripdata_v2.csv')






