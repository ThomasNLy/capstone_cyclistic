library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
directory <- "./summary_results_2020"
yearly_stat_summary_csv <- read_csv(file.path(directory, "all_usertypes_stats_summary.csv"))
casuals_members_weekly_ride_stats_csv <- read_csv(file.path(directory, "casuals_members_weekly_ride_stats.csv"))
yearly_trip_length_summary_df <- data.frame(
  user_type = c("members", "casuals"),
  trip_length = c(yearly_stat_summary_csv$members_average_trip_length_min, yearly_stat_summary_csv$casuals_average_trip_length_min)
)

# bar graph for comparing casuals vs members yearly ride length average
bar_graph_usertypes_average_ride_length <- ggplot(
  data = yearly_trip_length_summary_df
) +
  geom_col(mapping = aes(
    x = user_type, y = trip_length, fill = user_type
  )) +
  labs(
    title = "User Types Average Ride Length 2020",
    x = "User Types",
    y = "Ride Length (Min)",
    fill = "User Type"
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 5)))

#------------------weekly comparison-----------

day_of_week_col <- factor(casuals_members_weekly_ride_stats_csv$week_day_column,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)
print(day_of_week_col)
casuals_weekly_ride_average_df <- data.frame(
  day_of_week = day_of_week_col,
  ride_length = casuals_members_weekly_ride_stats_csv$casuals_weekly_ride_average,
  user_type = rep("casual", 7)
)

members_weekly_ride_average_df <- data.frame(
  day_of_week = day_of_week_col,
  ride_length = casuals_members_weekly_ride_stats_csv$members_weekly_ride_average,
  user_type = rep("member", 7)
)
weekly_ride_average_df <- rbind(casuals_weekly_ride_average_df, members_weekly_ride_average_df)
# weekly_ride_average_df <- weekly_ride_average_df %>% rename(day_of_week = week_day_column)
View(weekly_ride_average_df)


area_graph_average_weekly_ride_length <- ggplot(
  weekly_ride_average_df,
  aes(
    x = day_of_week,
    y = ride_length,
    fill = user_type,
    group = user_type
  )
) +
  scale_y_continuous(
    breaks = seq(0, round(max(weekly_ride_average_df$ride_length), -1) + 10, by = 10),
  ) +
  geom_area(position = "identity", alpha = 0.7) +
  geom_line(size = 1, stat = "identity", color = "#6b6b6b") +
  geom_point(size = 2, color = "#6b6b6b", stat = "identity") +
  coord_fixed(ratio = 0.05) +
  labs(
    x = "Day of the Week",
    y = "Ride Length (Min)",
    title = "Weekly Ride Length 2020: Members vs Casuals"
  )

casuals_weekly_ride_count_df <- data.frame(
  day_of_week = day_of_week_col,
  ride_count = casuals_members_weekly_ride_stats_csv$casuals_weekly_ride_count,
  user_type = rep("casual", 7)
)

members_weekly_ride_count_df <- data.frame(
  day_of_week = day_of_week_col,
  ride_count = casuals_members_weekly_ride_stats_csv$members_weekly_ride_count,
  user_type = rep("member", 7)
)


weekly_ride_count_df <- rbind(casuals_weekly_ride_count_df, members_weekly_ride_count_df)
bar_graph_user_types_ride_count <- ggplot(data = weekly_ride_count_df) +
  geom_col(
    mapping = aes(
      x = day_of_week, y = ride_count, fill = user_type
    ),
    position = "dodge"
  ) +
  scale_y_continuous(
    labels = function(y) y / 1000,
    breaks = seq(0, max(weekly_ride_count_df$ride_count) + 100000, by = 50000),
    expand = expansion(add = c(0, 20000))
  ) +
  labs(
    title = "Weekly Ride Count 2020: Members vs Casuals",
    x = "Day of the Week",
    y = "Ride Count (Thousands)",
    fill = "User Type"
  )



print(bar_graph_usertypes_average_ride_length)
print(area_graph_average_weekly_ride_length)
print(bar_graph_user_types_ride_count)
ggsave("user types weekly ride length 2020.png", path = directory, plot = bar_graph_usertypes_average_ride_length, width = 1500, height = 1700, units = "px", dpi = 300)
ggsave("weekly ride length average 2020.png", path = directory, plot = area_graph_average_weekly_ride_length, width = 2000, height = 1500, units = "px", dpi = 300)
ggsave("weekly ride count 2020.png", path = directory, plot = bar_graph_user_types_ride_count, width = 2000, height = 1500, units = "px", dpi = 300)
