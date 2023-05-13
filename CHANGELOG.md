# Changelog
This file contains the notable changes to the project

<br />

# Versão 1.0.0 (05-03-2023)

## New
    - Created a new dataset called "incorrect_time" to put 101 records where the values from started_at are higher 
      than values from ended_at.
    - Added column "date" to check the date of the trip
    - Added column "month" to check the month of the trip
    - Added column "day" to check the day of the trip
    - Added column "year" to check the year of the trip
    - Added column "day_of_week" to check the weekday
    - Added column "ride_length" to check the trip duration
    - Added column "start_hour" to check the start hour of the trip
    - Created a tripdata_v2 dataset backup
    
    
## Changes
    - Merged 12 datasets into one
    - Sorting the "started_at" column 
    - Changed data type of "ride_length" column  
    - Removal of negative values in "ride_length" column
    - Removal of values below 60 seconds in "ride_length" column
    - Deleted of "start_lat", "start_lng", "end_lat" and "end_lng" columns
    - Replaced empty data in (start_station_name, start_station_id, end_station_name, end_station_id) to missing data 
      (NAs)
    - Removal of missing data (NAs)
    
    
## Fixes
    - Fixed values in "started_at" and "ended_at" where 101 rows did not match with correct time.
    
