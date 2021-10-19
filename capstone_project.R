
##First installing and loading the packages required to read, and clean data

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("readr")
library(tidyverse)
library(janitor)
library(lubridate)
library(readr)

##then import 12 months data files from https://divvy-tripdata.s3.amazonaws.com/index.html
##while renaming each file into something more simple
df1<-read.csv("202004-divvy-tripdata.csv")
df2<-read.csv("202005-divvy-tripdata.csv")
df3<-read.csv("202006-divvy-tripdata.csv")
df4<-read.csv("202007-divvy-tripdata.csv")
df5<-read.csv("202008-divvy-tripdata.csv")
df6<-read.csv("202009-divvy-tripdata.csv")
df7<-read.csv("202010-divvy-tripdata.csv")
df8<-read.csv("202011-divvy-tripdata.csv")
df9<-read.csv("202012-divvy-tripdata.csv")
df10<-read.csv("202101-divvy-tripdata.csv")
df11<-read.csv("202102-divvy-tripdata.csv")
df12<-read.csv("202103-divvy-tripdata.csv")

#now that we have all 12 data files imported, we want to combine them all into one dataframe using rbind, which stands for row bind
bike_rides<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

#now we do data cleaning by removing nulls in rows and columns
bike_rides<-janitor::remove_empty(bike_rides,which=c("cols"))
bike_rides<-janitor::remove_empty(bike_rides,which=c("rows"))

##this shows the exact same number of columns and rows so therefore we didn't have duplicates to begin with
#we also do not want and na values
bike_rides<-na.omit(bike_rides)
#okay so this did remove some rows, we went down from 3489748 rows to 3354514 rows

##open the details in of your bike_rides table in the environment, this shows you what type of data is recorded, as you can see, the started at and ended at are charactor data, we want to convert to date format
bike_rides$started_at<-lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at<-lubridate::ymd_hms(bike_rides$ended_at)
# okay this didn't work, lets try
lubridate::ymd_hms(bike_rides$)
##nope! do not do this! it will omit every entry
##rerun the binding line of code

##okay lets try to convert chr to date a different way
bike_rides$started_at<-as.datetime(bike_rides$started_at)
#no this doesn't work either
#at this point I'm wondering if changing this could be done easier in excel and then to load it back into r

bike_rides$Ymd<-as.Date(bike_rides$started_at)

#WHAT AM I DOING WRONGG??
#okay this is it for today

#new day, trying lubridate again, still doesn't so i'm going to try changing each tables dates, then binding them into one

df12
df12$started_at<-lubridate::ymd_hms(df12$started_at)
# great this works, so I'm going to do it for the rest
df11$started_at<-lubridate::ymd_hms(df11$started_at)
df10$started_at<-lubridate::ymd_hms(df10$started_at)
df9$started_at<-lubridate::ymd_hms(df9$started_at)
df8$started_at<-lubridate::ymd_hms(df8$started_at)
df7$started_at<-lubridate::ymd_hms(df7$started_at)
df6$started_at<-lubridate::ymd_hms(df6$started_at)
df5$started_at<-lubridate::ymd_hms(df5$started_at)
df4$started_at<-lubridate::ymd_hms(df4$started_at)
df3$started_at<-lubridate::ymd_hms(df3$started_at)
df2$started_at<-lubridate::ymd_hms(df2$started_at)
df1$started_at<-lubridate::ymd_hms(df1$started_at)

#even though the code is saying "all formates failed to parse.no formats found" it does work
#so now converting ended at
df12$ended_at<-lubridate::ymd_hms(df12$ended_at)
df11$ended_at<-lubridate::ymd_hms(df11$ended_at)
df10$ended_at<-lubridate::ymd_hms(df10$ended_at)
df9$ended_at<-lubridate::ymd_hms(df9$ended_at)
df8$ended_at<-lubridate::ymd_hms(df8$ended_at)
df7$ended_at<-lubridate::ymd_hms(df7$ended_at)
df6$ended_at<-lubridate::ymd_hms(df6$ended_at)
df5$ended_at<-lubridate::ymd_hms(df5$ended_at)
df4$ended_at<-lubridate::ymd_hms(df4$ended_at)
df3$ended_at<-lubridate::ymd_hms(df3$ended_at)
df2$ended_at<-lubridate::ymd_hms(df2$ended_at)
df1$ended_at<-lubridate::ymd_hms(df1$ended_at)

#same warning message, but again works, so now we will bind

#no doesn't work

#okay lets start from the top, loaded files again and combine
#trying to format a diff way

bike_rides$started_at<-as.POSIXct(bike_rides$started_at,"%Y-%m-%d %H:%M:%S")
##omg this workss!
bike_rides$ended_at<-as.POSIXct(bike_rides$ended_at,"%Y-%m-%d %H:%M:%S")
#wow now i can get onto the next thing, which is creating a column that represents the ride time

bike_rides<-bike_rides %>%
  mutate(ride_duration=as.numeric(bike_rides$ended_at-bike_rides$started_at)/60)
#oKay no pipe funtion, so lets try installing dplyr
install.packages("dplyr")
library(dplyr)
#reran mutate code
#works!you can check now that your table has 14 variables(columns)
#now lets looks at the average of ride times

summary(bike_rides$ride_duration)
#this gives you nonsensical numbers, but looks like the average(mean)is ride is 24.55


#now we want to see what month of the year has the highest rides, so we need to separate year and month columns
#of our columns the only ones that have dates are the started and ended at, so lets separate them

bike_rides<-bike_rides%>%
  mutate(year_month=paste(strftime(bike_rides$started_at,"%Y"),
                          "-",
                          strftime(bike_rides$started_at,"%m"),
                          paste("(",strftime(bike_rides$started_at,"%B"),")",sep=" ")))
#not finding pipe again so lets load dplyr again
#pipe not found...lets try another way
#i tried it without the pipe, saying mutate cant be applied to an object of class chr, however the coloumns are in date format so I'm confused

install.packages("magrittr")
#no this didn't work either, I have to find another way to seperate
#ahh i'm a moron I was using the < instead of > for the pipe, only took an hour to figure out
#okay so the year and month have been put into a new coloumn, onto next task, to seperate weekdays

bike_rides<-bike_rides%>%
  mutate(weekday=paste(strftime(bike_rides$ended_at,"%u"),
                       "-",
                       strftime(bike_rides$ended_at,"%A")))
unique(bike_rides$weekday)
##yessss this works too, so at this point we have 16 variables(columns)
#lets also create one more column with the start hour of bike rides

bike_rides<-bike_rides%>%
  mutate(start_hour=strftime(bike_rides$ended_at,"%H"))
unique(bike_rides$start_hour)
#wow, okay I'm feeling soo much better now, so I believe my cleaning step is done, I don't think I have to rename any columns, they are pretty clear as to what they represent
#now save this clean dataset 

bike_rides%>%
  write.csv("bike_rides_clean.csv")
#okay I'm calling it a day

#now i have to change the data type on a couple more columns, so upload the cleaned dataset
#this takes a couple of minutes
View(bike_rides_clean)

#okay so it isn't loading but in the environment I still have my obs and 18 variables,
#in the environment, i have the start station name, start station id, end station name and end station id as
#chr so i dont need to change this.

#so now i want to remove ride lengths less than 0
bike_rides_clean<-bike_rides_clean %>%
  filter(!(ride_duration <0))
#okay so pipe doesnt work again, so I have to load up dplyr
library(dplyr)
# k ran lines 159 and 160 again and now works, now i have 3351199 obs instead of 3354514

#now i'm sure i have no nulls or ride durations less than 0, now i want to remove duplicates
#i want to make sure that the ride_id has no duplicates this is a unique character to each rider 

ride_id_check<-bike_rides_clean %>%
  count(ride_id)%>%
  filter(n > 1)
#this returns obs of 2 varibales- I'm not sure what this means, but I think this means that there are no duplicates

#now i want to understand what and how many types of bikes there is 

unique(bike_rides_clean$rideable_type)

#this returns 3 options:docked, electric and classic

#i think i should separate year and month columns
#year

bike_rides_clean$year <-format(
  bike_rides_clean$started_at,
  "%Y"
)

#month
bike_rides_clean$month <-format(
  bike_rides_clean$started_at,
  "%m"
)

#okay now i want to remove an extra column because it has redundant info

select(bike_rides_clean,-year_month)
#hum no this doesnt work, lets google

bike_rides_clean %>%
  select(-year_month)
 #okay this doesn't work either....I still have 20 columns

#okay lets load dplyr again

library(dplyr)
#nope...

bike_rides_clean %>%
  select(- year_month)

#nope, i dont want to go on before i clean this up, ill come back to this later

#i want to see how many stations there are

station_name_check<-bike_rides_clean %>%
  group_by(start_station_name) %>%
  count(start_station_name)
#wow, there is 706 stations

#now i want to know what stations where used at each month, so my data starts in april of 2020 to march of 2021

April_2020_filter<-bike_rides_clean %>%
  filter(
    month =="04"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
# great so i'm going to do this for all months
May_2020_filter<-bike_rides_clean %>%
  filter(
    month =="05"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
June_2020_filter<-bike_rides_clean %>%
  filter(
    month =="06"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
July_2020_filter<-bike_rides_clean %>%
  filter(
    month =="07"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
August_2020_filter<-bike_rides_clean %>%
  filter(
    month =="08"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
September_2020_filter<-bike_rides_clean %>%
  filter(
    month =="09"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
October_2020_filter<-bike_rides_clean %>%
  filter(
    month =="10"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
November_2020_filter<-bike_rides_clean %>%
  filter(
    month =="11"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
December_2020_filter<-bike_rides_clean %>%
  filter(
    month =="12"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
January_2021_filter<-bike_rides_clean %>%
  filter(
    month =="01"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
February_2021_filter<-bike_rides_clean %>%
  filter(
    month =="02"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
March_2021_filter<-bike_rides_clean %>%
  filter(
    month =="03"
  ) %>%
  group_by(
    start_station_name
  ) %>%
  count(start_station_name)
#okay so now I've made dataframes with each month and also how many times the station was used




  )







         
         






