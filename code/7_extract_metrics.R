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

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "location", "id", "year"))
write_rds(df_metrics, str_c(.path$dat_process, "metrics.rds"))