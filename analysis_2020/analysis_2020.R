library(tidyverse)
library(readr)
directory <- "./data_2020"
q1 <- read_csv(file.path(directory, "Divvy_trips_2020_Q1.csv"))
april_data <- read_csv(file.path(directory, "202004-divvy-tripdata.csv"))
may_data <- april_data <- read_csv(file.path(directory, "202005-divvy-tripdata.csv"))
june_data <- read_csv(file.path(directory, "202006-divvy-tripdata.csv"))
july_data <- read_csv(file.path(directory, "202007-divvy-tripdata.csv"))
aug_data <- read_csv(file.path(directory, "202008-divvy-tripdata.csv"))
sept_data <- read_csv(file.path(directory, "202009-divvy-tripdata.csv"))
oct_data <- read_csv(file.path(directory, "202010-divvy-tripdata.csv"))
nov_data <- read_csv(file.path(directory, "202011-divvy-tripdata.csv"))
dec_data <- read_csv(file.path(directory, "202012-divvy-tripdata.csv"))

q2 <- rbind(april_data, may_data, june_data)
q3 <- rbind(july_data, aug_data, sept_data)
q4 <- rbind(oct_data, nov_data, dec_data)

print("-----q1 columns------")
print(colnames(q1))


print("-----q2 columns------")
print(colnames(q2))
print("-----q3 columns------")
print(colnames(q3))
print("-----q4 columns------")
print(colnames(q4))
# yearly_df <- rbind(
#   q1,
#   april_data, may_data,
#   june_data, july_data,
#   aug_data, sept_data,
#   oct_data, nov_data,
#   dec_data
# )
yearly_df <- rbind(
  q1, q2, q3, q4
)
View(head(yearly_df, 500))


print("------------missing values------")
na_counts <- colSums(is.na(yearly_df))
print(na_counts)
print("------------------------------")

print(unique(yearly_df$member_casual)) # checking if there are any incorrect values

# checking if dates are swapped
wrong_dates <- yearly_df %>%
  filter(as.POSIXct(ended_at) < as.POSIXct(started_at))
sprintf("num dates swapped: %f", nrow(wrong_dates)) %>% print()

# cleaning data, in case info added in and dates are swapped
temp <- yearly_df %>%
  mutate(
    started_at_temp = if_else(as.POSIXct(ended_at) < as.POSIXct(started_at), ended_at, started_at),
    ended_at_temp = if_else(as.POSIXct(ended_at) < as.POSIXct(started_at), started_at, ended_at),
  )


yearly_df$started_at <- temp$started_at_temp
yearly_df$ended_at <- temp$ended_at_temp


yearly_df$ride_length_minutes <- difftime(
  yearly_df$ended_at,
  yearly_df$started_at,
  units = "mins"
) %>% round(digits = 2)
yearly_df$day_of_week <- weekdays(yearly_df$ended_at)


yearly_average_ride_length_min <- mean(yearly_df$ride_length_minutes) %>% round(digits = 2)
yearly_max_ride_length_min <- max(yearly_df$ride_length_minutes) %>% round(digits = 2)
yearly_day_of_week_ride_count_df <- table(yearly_df$day_of_week) %>% as.data.frame()
colnames(yearly_day_of_week_ride_count_df) <- c("day_of_week", "count")
yearly_day_of_week_ride_count_df$day_of_week <- as.character(yearly_day_of_week_ride_count_df$day_of_week)
View(yearly_day_of_week_ride_count_df)
day_of_week_index <- which.max(yearly_day_of_week_ride_count_df$count)

#---------------YEARLY STATS------------------
yearly_day_of_week_mode <- yearly_day_of_week_ride_count_df[day_of_week_index, ]
yearly_day_of_week_mode_name <- yearly_day_of_week_mode[1, 1]
sprintf("average ride length (min): %f", yearly_average_ride_length_min) %>% print()
sprintf("max ride length (min): %f", yearly_max_ride_length_min) %>% print()
sprintf("mode day of the week: %s", yearly_day_of_week_mode[1, 1]) %>% print()





#---------------------MEMBERS VS CASUAL STATS----------------

casuals_df <- yearly_df[yearly_df$member_casual == "casual", ]
members_df <- yearly_df[yearly_df$member_casual == "member", ]
View(head(casuals_df, 20))
View(head(members_df, 20))

casuals_average_trip_length_min <- mean(casuals_df$ride_length_minutes) %>% round(digits = 2)
members_average_trip_length_min <- mean(members_df$ride_length_minutes) %>% round(digits = 2)
sprintf("casuals average ride length (min) : %f", casuals_average_trip_length_min) %>% print()
sprintf("members average ride length (min) : %f", members_average_trip_length_min) %>% print()

#--------------memebers vs casuals weekly stats----------------

week_day_column <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#--------casuals ride stats-----------
casuals_monday_ride_average <- casuals_df %>%
  filter(day_of_week == "Monday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

casuals_tuesday_ride_average <- casuals_df %>%
  filter(day_of_week == "Tuesday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)


casuals_wednesday_ride_average <- casuals_df %>%
  filter(day_of_week == "Wednesday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)


casuals_thursday_ride_average <- casuals_df %>%
  filter(day_of_week == "Thursday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)


casuals_friday_ride_average <- casuals_df %>%
  filter(day_of_week == "Friday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)


casuals_saturday_ride_average <- casuals_df %>%
  filter(day_of_week == "Saturday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)


casuals_sunday_ride_average <- casuals_df %>%
  filter(day_of_week == "Sunday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

casuals_weekly_ride_average <- c(
  casuals_monday_ride_average,
  casuals_tuesday_ride_average,
  casuals_wednesday_ride_average,
  casuals_thursday_ride_average,
  casuals_friday_ride_average,
  casuals_saturday_ride_average,
  casuals_sunday_ride_average
)


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

casuals_weekly_ride_count <- c(
  casuals_monday_ride_count,
  casuals_tuesday_ride_count,
  casuals_wednesday_ride_count,
  casuals_thursday_ride_count,
  casuals_friday_ride_count,
  casuals_saturday_ride_count,
  casuals_sunday_ride_count
)


#------memebers ride stats--------

members_monday_ride_average <- members_df %>%
  filter(day_of_week == "Monday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_tuesday_ride_average <- members_df %>%
  filter(day_of_week == "Tuesday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_wednesday_ride_average <- members_df %>%
  filter(day_of_week == "Wednesday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_thursday_ride_average <- members_df %>%
  filter(day_of_week == "Thursday") %>%
  select(ride_length_minutes) %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_friday_ride_average <- members_df %>%
  filter(day_of_week == "Friday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_saturday_ride_average <- members_df %>%
  filter(day_of_week == "Saturday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_sunday_ride_average <- members_df %>%
  filter(day_of_week == "Sunday") %>%
  pull(ride_length_minutes) %>%
  mean() %>%
  round(digits = 2)

members_weekly_ride_average <- c(
  members_monday_ride_average,
  members_tuesday_ride_average,
  members_wednesday_ride_average,
  members_thursday_ride_average,
  members_friday_ride_average,
  members_saturday_ride_average,
  members_sunday_ride_average
)

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

members_weekly_ride_count <- c(
  members_monday_ride_count,
  members_tuesday_ride_count,
  members_wednesday_ride_count,
  members_thursday_ride_count,
  members_friday_ride_count,
  members_saturday_ride_count,
  members_sunday_ride_count
)

#-----------------DATA SUMMARIZED DATA FRAMES-----------------
all_usertypes_stats_summary_df <- data.frame(
  all_usertypes_average_ride_length_min = yearly_average_ride_length_min,
  all_usertypes_max_ride_length_min = yearly_max_ride_length_min,
  casuals_average_trip_length_min,
  members_average_trip_length_min,
  mode_day_of_week = yearly_day_of_week_mode_name
)

casuals_members_weekly_ride_stats_df <- data.frame(
  week_day_column,
  casuals_weekly_ride_average,
  members_weekly_ride_average,
  casuals_weekly_ride_count,
  members_weekly_ride_count
)

View(all_usertypes_stats_summary_df)
View(casuals_members_weekly_ride_stats_df)
