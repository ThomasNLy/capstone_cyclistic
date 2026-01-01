library(tidyverse)
library(dplyr)
library(hms) # used to format time
q1 <- read_csv("./data_2019/Divvy_Trips_2019_Q1.csv")
q2 <- read_csv("./data_2019/Divvy_Trips_2019_Q2.csv")
q3 <- read_csv("./data_2019/Divvy_Trips_2019_Q3.csv")
q4 <- read_csv("./data_2019/Divvy_Trips_2019_Q4.csv")

#-------------checking if all the column names are the same and consistent to combine them
print("q1 columns")
print(colnames(q1))
print("------------")
print("q2 columns")
print(colnames(q2))
print("------------")
print("q3 columns")
print(colnames(q3))
print("------------")
print("q4 columns")
print(colnames(q4))
print("------------")
#-----------------------------------

#-------renaming the column names to match the others----
colnames(q2) <- c(
  "trip_id", "start_time", "end_time", "bikeid",
  "tripduration", "from_station_id", "from_station_name", "to_station_id",
  "to_station_name", "usertype", "gender", "birthyear"
)
#-----------------------------
yearly_df <- rbind(q1, q2, q3, q4) # combining them into 1 data set

#--------counting number of blank values
# na_counts <- colSums(is.na(yearly_df))
# print(na_counts)

# renaming values in the columns to keep with consistency of terms used
# in outline
yearly_df$usertype[yearly_df$usertype == "Subscriber"] <- "member"
yearly_df$usertype[yearly_df$usertype == "Customer"] <- "casual"

# renaming the invalid numbers to be NA instead to keep with consistency
yearly_df$birthyear[yearly_df$birthyear == "Invalid Number"] <- "NA"

# check if dates are swapped
# wrong <- yearly_df %>% filter(difftime(end_time, start_time) < 0)
# a <- sum(wrong$ride_length_minutes < 0)
# print("dates backwards")
# print(a)
# View(head(wrong, 100))

wrong <- yearly_df %>% filter(
  as.POSIXct(end_time) < as.POSIXct(start_time)
)
View(head(wrong, 100))

# cleaning data, in case info added in and dates are swapped
# temp <- yearly_df %>%
#   mutate(
#     started_at_temp = if_else(difftime(end_time, start_time, units = "mins") < 0, end_time, start_time),
#     ended_at_temp = if_else(difftime(end_time, start_time, units = "mins") < 0, start_time, end_time),
#   )
temp <- yearly_df %>%
  mutate(
    started_at_temp = if_else(as.POSIXct(end_time) < as.POSIXct(start_time), end_time, start_time),
    ended_at_temp = if_else(as.POSIXct(end_time) < as.POSIXct(start_time), start_time, end_time),
  )

yearly_df$start_time <- temp$started_at_temp
yearly_df$end_time <- temp$ended_at_temp
ride_length_secs <- difftime(yearly_df$end_time, yearly_df$start_time, units = "secs")
day_of_week <- weekdays(yearly_df$end_time)
yearly_df$ride_length_minutes <- difftime(yearly_df$end_time, yearly_df$start_time, units = "mins") %>% round(digits = 2)
# formatting trip length, requires time in secs
yearly_df$ride_length_formatted <- as_hms(ride_length_secs)
yearly_df$day_of_week <- day_of_week




View(head(yearly_df, 100))

mean_ride_length <- mean(ride_length_secs) %>% as_hms()
max_ride_length <- max(ride_length_secs) %>% as_hms()
day_of_week_ride_count <- table(yearly_df$day_of_week)
print("weekly trip count all usertypes:")
print(day_of_week_ride_count)

# conver to data frame to get the name of the day of the week from contingency table
mode_day_of_week <- which.max(day_of_week_ride_count) %>%
  as.data.frame()
mode_day_of_week <- rownames(mode_day_of_week)[1] # name for day of the week is used as the row identifier rather than a number
day_of_week_ride_count_df <- as.data.frame(day_of_week_ride_count)


sprintf("mean ride length: %s", mean_ride_length) %>% print()
sprintf("max ride length: %s", max_ride_length) %>% print()
print("------------------------")
sprintf("mode day of the week: %s", mode_day_of_week) %>% print()
print("------------------------")


#-----------members average ride length overall-------
members_df <- yearly_df[yearly_df$usertype == "member", ]
casuals_df <- yearly_df[yearly_df$usertype == "casual", ]
members_average_trip_length_secs <- mean(as.numeric(members_df$ride_length_minutes)) * 60
casuals_average_trip_length_secs <- mean(as.numeric(casuals_df$ride_length_minutes)) * 60
members_average_trip_length <- members_average_trip_length_secs %>%
  as_hms() %>%
  round_hms(digits = 0)

casuals_average_trip_length <- casuals_average_trip_length_secs %>%
  as_hms() %>%
  round_hms(digits = 0)
sprintf("member average ride length: %s", members_average_trip_length) %>% print()
sprintf("casual average ride length: %s", casuals_average_trip_length) %>% print()
trips_stats_summary_df <- data.frame(
  all_usertypes_average_ride_length = mean_ride_length,
  all_usertypes_max_ride_length = max_ride_length,
  members_average_ride_length = members_average_trip_length,
  casuals_average_ride_length = casuals_average_trip_length,
  mode_day_of_week
)
View(trips_stats_summary_df)



#------------DATA FRAMES WEEKLY AVERAGE USERTYPES: MEMEBERS, CASUALS-------------------
week_day_column <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#---------casual weekly average data frame------------
casuals_monday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Monday"]
) %>% round(digits = 2)
casuals_tuesday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Tuesday"]
) %>% round(digits = 2)
casuals_wednesday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Wednesday"]
) %>% round(digits = 2)
casuals_thursday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Thursday"]
) %>% round(digits = 2)
casuals_friday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Friday"]
) %>% round(digits = 2)
casuals_saturday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Saturday"]
) %>% round(digits = 2)
casuals_sunday_average <- mean(
  casuals_df$ride_length_minutes[casuals_df$day_of_week == "Sunday"]
) %>% round(digits = 2)
casuals_weekly_average_ride_length <- c(
  casuals_monday_average,
  casuals_tuesday_average,
  casuals_wednesday_average,
  casuals_thursday_average,
  casuals_friday_average,
  casuals_saturday_average,
  casuals_sunday_average
)
casuals_weekly_ride_average_df <- data.frame(
  day_of_week = week_day_column,
  ride_length_mins = casuals_weekly_average_ride_length
)
View(casuals_weekly_ride_average_df)

#---memebers weekly average data frame------------
members_monday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Monday"]
) %>% round(digits = 2)
members_tuesday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Tuesday"]
) %>% round(digits = 2)
members_wednesday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Wednesday"]
) %>% round(digits = 2)
members_thursday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Thursday"]
) %>% round(digits = 2)
members_friday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Friday"]
) %>% round(digits = 2)
members_saturday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Saturday"]
) %>% round(digits = 2)
members_sunday_average <- mean(
  members_df$ride_length_minutes[members_df$day_of_week == "Sunday"]
) %>% round(digits = 2)
members_weekly_average_ride_length <- c(
  members_monday_average,
  members_tuesday_average,
  members_wednesday_average,
  members_thursday_average,
  members_friday_average,
  members_saturday_average,
  members_sunday_average
)
members_weekly_ride_average_df <- data.frame(
  day_of_week = week_day_column,
  ride_length_mins = members_weekly_average_ride_length
)
View(members_weekly_ride_average_df)

members_casuals_weekly_ride_average_df <- data.frame(
  day_of_week = week_day_column,
  members_ride_length_mins = members_weekly_average_ride_length,
  casuals_ride_length_mins = casuals_weekly_average_ride_length
)
View(members_casuals_weekly_ride_average_df)

#------------DATA FRAMES WEEKLY RIDE COUNT USERTYPES: MEMEBERS, CASUALS-------------------
members_monday_ride_count <- members_df %>%
  filter(day_of_week == "Monday") %>%
  nrow()
members_tuesday_ride_count <- members_df %>%
  filter(day_of_week == "Tuesday") %>%
  nrow()
members_wednesday_ride_count <- members_df %>%
  filter(day_of_week == "Wednesday") %>%
  nrow()
members_thursday_ride_count <- members_df %>%
  filter(day_of_week == "Thursday") %>%
  nrow()
members_friday_ride_count <- members_df %>%
  filter(day_of_week == "Friday") %>%
  nrow()
members_saturday_ride_count <- members_df %>%
  filter(day_of_week == "Saturday") %>%
  nrow()
members_sunday_ride_count <- members_df %>%
  filter(day_of_week == "Sunday") %>%
  nrow()


casuals_monday_ride_count <- casuals_df %>%
  filter(day_of_week == "Monday") %>%
  nrow()
casuals_tuesday_ride_count <- casuals_df %>%
  filter(day_of_week == "Tuesday") %>%
  nrow()
casuals_wednesday_ride_count <- casuals_df %>%
  filter(day_of_week == "Wednesday") %>%
  nrow()
casuals_thursday_ride_count <- casuals_df %>%
  filter(day_of_week == "Thursday") %>%
  nrow()
casuals_friday_ride_count <- casuals_df %>%
  filter(day_of_week == "Friday") %>%
  nrow()
casuals_saturday_ride_count <- casuals_df %>%
  filter(day_of_week == "Saturday") %>%
  nrow()
casuals_sunday_ride_count <- casuals_df %>%
  filter(day_of_week == "Sunday") %>%
  nrow()

members_weekly_ride_count <- c(
  members_monday_ride_count, members_tuesday_ride_count,
  members_wednesday_ride_count, members_thursday_ride_count,
  members_friday_ride_count, members_saturday_ride_count,
  members_sunday_ride_count
)

casuals_weekly_ride_count <- c(
  casuals_monday_ride_count, casuals_tuesday_ride_count,
  casuals_wednesday_ride_count, casuals_thursday_ride_count,
  casuals_friday_ride_count, casuals_saturday_ride_count,
  casuals_sunday_ride_count
)
members_casuals_weekly_ride_count_df <- data.frame(
  week_day_column,
  members_weekly_ride_count,
  casuals_weekly_ride_count
)
View(members_casuals_weekly_ride_count_df)

#-------------------------------DEMOGRAPHIC DATA FRAME: BIRTH YEAR------------------
casuals_birth_year_df <- casuals_df %>%
  filter(birthyear != "NA") %>%
  count(birthyear)

View(casuals_birth_year_df)

members_birth_year_df <- members_df %>%
  filter(birthyear != "NA") %>%
  count(birthyear)

View(members_birth_year_df)


#---------DEMOGRAPHIC DATA FRAME: GENDER------------------
casuals_gender_df <- casuals_df %>%
  filter(gender != "NA") %>%
  count(gender)
View(casuals_gender_df)

members_gender_df <- members_df %>%
  filter(gender != "NA") %>%
  count(gender)
View(members_gender_df)
