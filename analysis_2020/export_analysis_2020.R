# make directory ahead of time

source("./analysis_2020/analysis_2020.R")
# export to csv
write_csv(yearly_day_of_week_ride_count_df, "./summary_results_2020/yearly_day_of_week_ride_count.csv")
write_csv(all_usertypes_stats_summary_df, "./summary_results_2020/all_usertypes_stats_summary.csv")
write_csv(casuals_members_weekly_ride_stats_df, "./summary_results_2020/casuals_members_weekly_ride_stats.csv")
