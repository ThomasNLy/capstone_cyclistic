source("./analysis_2019/analysis_2019.R")
# export to csv
write_csv(trips_stats_summary_df, "./summary_results_2019/trips_stats_summary.csv")
write_csv(members_casuals_weekly_ride_count_df, "./summary_results_2019/members_casuals_weekly_ride_count.csv")
write_csv(members_casuals_weekly_ride_average_df, "./summary_results_2019/members_casuals_weekly_ride_average.csv")
write_csv(members_birth_year_df, "./summary_results_2019/members_birth_year.csv")
write_csv(casuals_birth_year_df, "./summary_results_2019/casuals_birth_year.csv")
write_csv(casuals_gender_df, "./summary_results_2019/casuals_gender.csv")
write_csv(members_gender_df, "./summary_results_2019/members_gender.csv")
