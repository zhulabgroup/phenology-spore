df_smooth <- read_rds(str_c(.path$dat_process, "fill_smooth_to2021.rds"))

# peak concentration & date
df_peak <- df_smooth %>%
  drop_na(count_smooth) %>%
  group_by(lat, lon, location, id, year) %>%
  filter(count_smooth == max(count_smooth)) %>%
  summarise(
    peak = head(count_smooth, 1),
    peak_doy = head(doy, 1)
  )

# integral in time window
df_integral <- df_smooth %>%
  drop_na(count_smooth) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  group_by(lat, lon, location, id, year) %>%
  summarise(integral = sum(count_smooth) / n() * 214)

# growing season
df_season <- df_smooth %>%
  drop_na(count_smooth) %>%
  filter(count_smooth > 0) %>% # the number of zero can be variable dependent on the sampling time, confounding the percentile.
  group_by(lat, lon, location, id, year) %>%
  filter(count_smooth >= quantile(count_smooth, 0.3, na.rm = T)) %>%
  summarise(
    sos = min(doy),
    eos = max(doy)
  ) %>%
  mutate(los = eos - sos)

# # this code doesn't work when the time series is all 0
# # you used 30 percent rather than 30 percentile
# # in the line threshold <- data$count_smooth[i - 1], you extracted the concentration of the day before crossing the threshold, but the daily fluctuation makes this an inconsistent threshold
# threshold_df <- df_smooth %>%
#   filter(!is.na(count_smooth)) %>%
#   group_by(location, id, year) %>%
#   summarise(threshold_inte = 0.3 * sum(count_smooth))
# # start_date & end_date
# start_date <- c()
# end_date <- c()
# for (n in 1:194) {
#   inte <- 0
#   data <- df_smooth %>%
#     filter(id == threshold_df$id[n]) %>%
#     filter(year == threshold_df$year[n]) %>%
#     filter(!is.na(count_smooth)) %>%
#     arrange(count_smooth)
#   for (i in 1:nrow(data)) {
#     inte <- inte + data$count_smooth[i]
#     if (inte > threshold_df$threshold_inte[n]) {
#       break
#     }
#   }
#   threshold <- data$count_smooth[i - 1]
#   data2 <- df_smooth %>%
#     filter(id == threshold_df$id[n]) %>%
#     filter(year == threshold_df$year[n]) %>%
#     filter(!is.na(count_smooth)) %>%
#     filter(count_smooth > threshold) %>%
#     arrange(doy)
#   start_date[n] <- data2$date[1]
#   end_date[n] <- data2$date[nrow(data2)]
# }
# threshold_df$start <- as.Date(start_date)
# threshold_df$end <- as.Date(end_date)
# threshold_df <- threshold_df %>%
#   rename("start_date" = "start") %>%
#   rename("end_date" = "end") %>%
#   dplyr::select(-threshold_inte)
# parameters_df <- full_join(parameters_df, threshold_df, by = c("location" = "location", "id" = "id", "year" = "year"))
# write_rds(parameters_df, "~/spore_phenology/data/parameters.rds")

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "location", "id", "year"))
write_rds(df_metrics, str_c(.path$dat_process, "metrics.rds"))
