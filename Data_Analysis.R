install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("anytime")
install.packages("lubridate")
library(tidyverse)
library(janitor)
library(skimr)
library(anytime) 
library(hms)
library(lubridate)

getwd()

jan_data_2022<-read_csv("202201-divvy-tripdata.csv")
feb_data_2022<-read_csv("202202-divvy-tripdata.csv")
mar_data_2022<-read_csv("202203-divvy-tripdata.csv")
apr_data_2022<-read_csv("202204-divvy-tripdata.csv")
may_data_2022<-read_csv("202205-divvy-tripdata.csv")
jun_data_2022<-read_csv("202206-divvy-tripdata.csv")
jul_data_2022<-read_csv("202207-divvy-tripdata.csv")
aug_data_2021<-read_csv("202108-divvy-tripdata.csv")
sep_data_2021<-read_csv("202109-divvy-tripdata.csv")
oct_data_2021<-read_csv("202110-divvy-tripdata.csv")
nov_data_2021<-read_csv("202111-divvy-tripdata.csv")
dec_data_2021<-read_csv("202112-divvy-tripdata.csv")

str(jan_data_2022)
head(sep_data_2021$started_at)
head(jan_data_2022)


compare_df_cols(jan_data_2022,feb_data_2022,mar_data_2022,
                apr_data_2022,may_data_2022,jun_data_2022,
                jul_data_2022,aug_data_2021,sep_data_2021,
                oct_data_2021,nov_data_2021,dec_data_2021,return="mismatch")

#class(all_trips_data$start_station_id) 
#class(all_trips_data$end_station_id)

jan_data_2022<-mutate(jan_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
feb_data_2022<-mutate(feb_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
mar_data_2022<-mutate(mar_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
apr_data_2022<-mutate(apr_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
may_data_2022<-mutate(may_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
jun_data_2022<-mutate(jun_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
jul_data_2022<-mutate(jul_data_2022,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
aug_data_2021<-mutate(aug_data_2021,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
sep_data_2021<-mutate(sep_data_2021,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
oct_data_2021<-mutate(oct_data_2021,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
nov_data_2021<-mutate(nov_data_2021,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))
dec_data_2021<-mutate(dec_data_2021,end_station_id = as.character(end_station_id),
                      start_station_id = as.character(start_station_id))


all_trips_data <- rbind(jan_data_2022,feb_data_2022,mar_data_2022,
                        apr_data_2022,may_data_2022,jun_data_2022,
                        jul_data_2022,aug_data_2021,sep_data_2021,
                        oct_data_2021,nov_data_2021,dec_data_2021)

#removing the dataframes from the environment
remove(jan_data_2022,feb_data_2022,mar_data_2022,
       apr_data_2022,may_data_2022,jun_data_2022,
       jul_data_2022,aug_data_2021,sep_data_2021,
       oct_data_2021,nov_data_2021,dec_data_2021)



#remove the columns which are unused
all_trips_data <- all_trips_data %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))
all_trips_data <- all_trips_data %>%
  select(-c(start_station_id,end_station_id))

# ************** cleaning the data for Analysis ********************

#check all the columns after removing the unused columns
colnames(all_trips_data)

clean_names(all_trips_data) # to make sure the column names are unique and consistent

#check the cells if any consisting of 'N/A' values
sum(is.na(all_trips_data))
all_trips_data<-na.omit(all_trips_data)
#fill empty cells with na
#all_trips_data[all_trips_data==""]<-NA


#exporting the data frame into CSV file
write.csv(all_trips_data,"testdata.csv")


dim(all_trips_data)
str(all_trips_data)
summary(all_trips_data)
skim(all_trips_data)

#Fix the charToDate Error Using the anydate() Function
#any date function is used to convert our original character string to date class
#all_trips_data_filtered$started_at<-anydate(all_trips_data_filtered$started_at)
#all_trips_data_filtered$date <- as.Date(all_trips_data_filtered$started_at)  

#create columns for date,month,time,day,year,day of week ,hour,time
all_trips_data$date<-as.Date(all_trips_data$started_at)
all_trips_data$month <- format(as.Date(all_trips_data$date),"%m")
all_trips_data$day <- format(as.Date(all_trips_data$date),"%d")
all_trips_data$year <- format(as.Date(all_trips_data$date),"%Y")
all_trips_data$day_of_week <- format(as.Date(all_trips_data$date),"%A")

#adding time column
all_trips_data$time <- format(as.Date(all_trips_data$date),"%H:%M:%S")
all_trips_data$time <- as_hms((all_trips_data$started_at))  #hms library
all_trips_data$hour <- hour(all_trips_data$time) #from lubridate package


# calculating the "ride_length" and converting into factor to numeric so we can run calculations on the data
all_trips_data$ride_length<- difftime(all_trips_data$ended_at,all_trips_data$started_at,units='mins')
is.factor(all_trips_data$ride_length)
all_trips_data$ride_length <- as.numeric(as.character(all_trips_data$ride_length))
is.numeric(all_trips_data$ride_length)

#remove negative ride length
all_trips_data <- all_trips_data[!(all_trips_data$ride_length<0),]


#creating a new column to specify the part of the day
all_trips_data <-all_trips_data %>% mutate(time_of_day = 
                             case_when(hour == "0" ~ "Night",
                                       hour == "1" ~ "Night",
                                       hour == "2" ~ "Night",
                                       hour == "3" ~ "Night",
                                       hour == "4" ~ "Night",
                                       hour == "5" ~ "Night",
                                       hour == "6" ~ "Morning",
                                       hour == "7" ~ "Morning",
                                       hour == "8" ~ "Morning",
                                       hour == "9" ~ "Morning",
                                       hour == "10" ~ "Morning",
                                       hour == "11" ~ "Morning",
                                       hour == "12" ~ "Afternoon",
                                       hour == "13" ~ "Afternoon",
                                       hour == "14" ~ "Afternoon",
                                       hour == "15" ~ "Afternoon",
                                       hour == "16" ~ "Afternoon",
                                       hour == "17" ~ "Evening",
                                       hour == "18" ~ "Evening",
                                       hour == "19" ~ "Evening",
                                       hour == "20" ~ "Evening",
                                       hour == "21" ~ "Evening",
                                       hour == "22" ~ "Evening",
                                       hour == "23" ~ "Night")
)

#creating a column for different seasons in a year and mapping it with month
all_trips_data <-all_trips_data %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06" ~ "Summer",
                                                       month == "07" ~ "Summer",
                                                       month == "08" ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

# ******************* descriptive analysis ***********************

summary(all_trips_data$ride_length)

#Compare members and casual users
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual, FUN = mean)
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual, FUN = median)
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual, FUN = max)
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual + all_trips_data$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_data$day_of_week <- ordered(all_trips_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#now see the change
aggregate(all_trips_data$ride_length ~ all_trips_data$member_casual + all_trips_data$day_of_week, FUN = mean)

#exporting the data frame into CSV file
write.csv(all_trips_data,"testdata.csv")

