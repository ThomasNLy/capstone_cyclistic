library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
directory <- "./summary_results_2019"
trips_summary_csv <- read_csv(file.path(directory, "trips_stats_summary.csv"))
weekly_ride_average_csv <- read_csv(file.path(directory, "members_casuals_weekly_ride_average.csv"))
weekly_ride_count_csv <- read_csv(file.path(directory, "members_casuals_weekly_ride_count.csv"))
casuals_birth_year_csv <- read_csv(file.path(directory, "casuals_birth_year.csv"))
members_birth_year_csv <- read_csv(file.path(directory, "members_birth_year.csv"))
casuals_gender_csv <- read_csv(file.path(directory, "casuals_gender.csv"))
members_gender_csv <- read_csv(file.path(directory, "members_gender.csv"))
colnames(casuals_birth_year_csv)[2] <- "count"
colnames(members_birth_year_csv)[2] <- "count"
colnames(casuals_gender_csv)[2] <- "count"
colnames(members_gender_csv)[2] <- "count"
# View(trips_summary_csv)
# View(weekly_ride_average_csv)
# View(weekly_ride_count_csv)

members_ride_average_min <- (as.difftime(trips_summary_csv$members_average_ride_length, format = "%H:%M:%S, units = minutes") / 60) %>%
  as.numeric()
casuals_ride_average_min <- (as.difftime(trips_summary_csv$casuals_average_ride_length, format = "%H:%M:%S, units = minutes") / 60) %>%
  as.numeric()
all_usertypes_ride_length_df <- data.frame(
  user_type = c("members", "casuals"),
  ride_length = c(members_ride_average_min, casuals_ride_average_min)
)

# View(all_usertypes_ride_length_df)
bar_chart_usertypes_average_ride_length <- ggplot(
  data = all_usertypes_ride_length_df
) +
  geom_col(mapping = aes(
    x = user_type, y = ride_length, fill = user_type
  )) +
  labs(
    title = "User Types Average Ride Length 2019",
    x = "User Type",
    y = "Ride Length (Min)",
    fill = "User Type"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(all_usertypes_ride_length_df$ride_length) + 10, by = 10),
    expand = expansion(add = c(0, 5))
  )

weekly_ride_average_len <- nrow(weekly_ride_average_csv)
temp <- c("character", length = nrow(weekly_ride_average_csv))
for (i in 1:weekly_ride_average_len) {
  temp[i] <- "casual"
}
day_of_week <- factor(weekly_ride_average_csv$day_of_week,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)
casuals_weekly_ride_average_df <- data.frame(
  day_of_week = day_of_week,
  ride_length = c(weekly_ride_average_csv$casuals_ride_length_mins),
  user_type = temp
)

temp2 <- c("character", length = nrow(weekly_ride_average_csv))
for (i in 1:weekly_ride_average_len) {
  temp2[i] <- "member"
}
members_weekly_ride_average_df <- data.frame(
  day_of_week = day_of_week,
  ride_length = c(weekly_ride_average_csv$members_ride_length_mins),
  user_type = temp2
)


weekly_ride_average_df <- rbind(casuals_weekly_ride_average_df, members_weekly_ride_average_df)
# View(weekly_ride_average_df)
area_graph_average_weekly_ride_length <- ggplot(weekly_ride_average_df, aes(x = day_of_week, y = ride_length, fill = user_type, group = user_type)) +
  scale_y_continuous(
    breaks = seq(0, round(max(weekly_ride_average_df$ride_length), -1) + 10, by = 10)
  ) +
  geom_area(position = "identity", alpha = 0.7) +
  geom_line(linewidth = 1, stat = "identity", color = "#6b6b6b") +
  geom_point(size = 2, color = "#6b6b6b", stat = "identity") +
  coord_fixed(ratio = 0.05) +
  labs(
    x = "Day of the Week",
    y = "Ride Length (Min)",
    title = "Weekly Ride Length 2019: Members vs Casuals"
  )



#-------------Age  Demographic Line Graphs------------------
casuals_line_graph_age_demographic <- ggplot(casuals_birth_year_csv, aes(x = birthyear, y = count)) +
  geom_line(linewidth = 1, stat = "identity") +
  coord_fixed(ratio = 0.009) +
  labs(
    title = "Casual Users' Demographic (Age) 2019",
    x = "Birth Year",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = seq(round(min(casuals_birth_year_csv$birthyear), digits = -2) - 60, round(max(casuals_birth_year_csv$birthyear), digits = -2) + 10, by = 20),
    expand = c(0, 20)
  )


members_line_graph_age_demographic <- ggplot(members_birth_year_csv, aes(x = birthyear, y = count)) +
  geom_line(linewidth = 1, stat = "identity") +
  coord_fixed(ratio = 0.0007) +
  labs(
    title = "Members' Demographic (Age) 2019",
    x = "Birth Year",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = seq(round(min(members_birth_year_csv$birthyear), digits = -2) - 20, round(max(members_birth_year_csv$birthyear), digits = -2) + 20, by = 10),
    expand = c(0, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, round(max(members_birth_year_csv$count), digits = -2), by = 20000)
  )


#------------------------Gender demographic bar charts---------------

# compare bar charts side by side for casuals and members

casuals_female_count <- casuals_gender_csv$count[1]
casuals_male_count <- casuals_gender_csv$count[2]
casuals_female_percent <- (casuals_female_count / (casuals_male_count + casuals_female_count)) * 100
casuals_male_percent <- (casuals_male_count / (casuals_male_count + casuals_female_count)) * 100
casuals_female_percent <- round(casuals_female_percent, digits = 2)
casuals_male_percent <- round(casuals_male_percent, digits = 2)

members_female_count <- members_gender_csv$count[1]
members_male_count <- members_gender_csv$count[2]
members_female_percent <- (members_female_count / (members_male_count + members_female_count)) * 100
members_male_percent <- (members_male_count / (members_male_count + members_female_count)) * 100
members_female_percent <- round(members_female_percent, digits = 2)
members_male_percent <- round(members_male_percent, digits = 2)


gender_df <- data.frame(
  gender = c(casuals_gender_csv$gender, members_gender_csv$gender),
  percentage = c(casuals_female_percent, casuals_male_percent, members_female_percent, members_male_percent),
  usertype = c("casual", "casual", "member", "member")
)
View(gender_df)

# custom colour for the bars using scale_fill_manual
bar_chart_gender <- ggplot(gender_df, mapping = aes(x = gender, y = percentage, fill = usertype)) +
  geom_col(
    position = "dodge"
  ) +
  scale_fill_manual(values = c("#57bd70", "#577cbd")) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "Gender Demographic: Members vs Casual Users",
    x = "Gender",
    y = "Percentage"
  ) +
  geom_text(aes(label = percentage, group = usertype, vjust = -1), position = position_dodge(width = 0.9))


#---------------weekly ride count members vs casuals--------------------

usertype_member <- c("character", length = nrow(weekly_ride_average_csv))
for (i in 1:weekly_ride_average_len) {
  usertype_member[i] <- "member"
}

members_ride_count <- vector("numeric", 7)
for (i in 1:7) {
  members_ride_count[i] <- weekly_ride_count_csv$members_weekly_ride_count[
    weekly_ride_count_csv$week_day_column == day_of_week[i]
  ]
}

members_weekly_ride_count_df <- data.frame(
  day_of_week,
  user_type = usertype_member,
  ride_count = members_ride_count
)

usertype_casual <- c("character", length = nrow(weekly_ride_average_csv))
for (i in 1:weekly_ride_average_len) {
  usertype_casual[i] <- "casual"
}
casuals_ride_count <- vector("numeric", 7)
for (i in 1:7) {
  casuals_ride_count[i] <- weekly_ride_count_csv$casuals_weekly_ride_count[
    weekly_ride_count_csv$week_day_column == day_of_week[i]
  ]
}

casuals_weekly_ride_count_df <- data.frame(
  day_of_week,
  user_type = usertype_casual,
  ride_count = casuals_ride_count
)


weekly_ride_count_df <- rbind(members_weekly_ride_count_df, casuals_weekly_ride_count_df)
# View(weekly_ride_count_df)

bar_chart_weekly_ride_count <- ggplot(
  data = weekly_ride_count_df
) +
  geom_col(mapping = aes(
    x = day_of_week, y = ride_count, fill = user_type
  ), position = "dodge") +
  labs(
    title = "Weekly Ride Count 2019: Members vs Casuals",
    x = "Day of the Week",
    y = "Ride Count"
  ) +
  scale_y_continuous(breaks = seq(0, max(weekly_ride_count_df$ride_count), by = 50000))

#-------------viewing graphs----------------
print(bar_chart_usertypes_average_ride_length)
print(area_graph_average_weekly_ride_length)
print(casuals_line_graph_age_demographic)
print(members_line_graph_age_demographic)
print(bar_chart_gender)
print(bar_chart_weekly_ride_count)
ggsave("user types average ride length 2019.png", path = directory, plot = bar_chart_usertypes_average_ride_length, width = 1500, height = 1700, units = "px", dpi = 300)
ggsave("weekly ride length average 2019.png", path = directory, plot = area_graph_average_weekly_ride_length, width = 2000, height = 1500, units = "px", dpi = 300)
ggsave("casual user type demographic 2019.png", path = directory, plot = casuals_line_graph_age_demographic, width = 2000, height = 2000, units = "px", dpi = 300)
ggsave("member user type demographic 2019.png", path = directory, plot = members_line_graph_age_demographic, width = 2000, height = 2000, units = "px", dpi = 300)
ggsave("weekly ride count 2019.png", path = directory, plot = bar_chart_weekly_ride_count, width = 2000, height = 1500, units = "px", dpi = 300)
ggsave("gender demographic 2019.png", path = directory, plot = bar_chart_gender, width = 2000, height = 1500, units = "px", dpi = 300)
