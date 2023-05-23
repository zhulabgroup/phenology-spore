# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
# df_sporeyr_a <- df_smooth %>%
#   filter(offset < 183) %>%
#   group_by(lat, lon, station, city, state, country, id, n) %>%
#   mutate(year_new = case_when(doy <= offset ~ (year - 1),
#                               TRUE ~ as.double(year))) %>%
#   mutate(doy_new = case_when(doy > offset ~ (doy - offset),
#                              TRUE ~ as.numeric(difftime(date, as.Date(paste0(year_new,"-01-01")) ) - offset + 1))) %>%
#   mutate(date_new = as.POSIXct(as.Date(doy_new, origin = paste0(year_new, "-01-01"))) -1) %>%
#   ungroup()
# 
# df_sporeyr_b <- df_smooth %>%
#   filter(offset >= 183) %>%
#   group_by(lat, lon, station, city, state, country, id, n) %>%
#   mutate(year_new = case_when(doy > offset ~ (year + 1),
#                               TRUE ~ as.double(year))) %>%
#   mutate(doy_new = case_when(doy <= offset ~ (doy + 365 - offset),
#                              TRUE ~ doy - offset)) %>%
#   mutate(date_new = as.POSIXct(as.Date(doy_new, origin = paste0(year_new, "-01-01"))) -1) %>%
#   ungroup()
# 
# df_sporeyr <- bind_rows(df_sporeyr_a, df_sporeyr_b) %>%
#   mutate(date_new = format(date_new, "%Y-%m-%d"))
# 
# write_rds(df_sporeyr, str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_peak <- df_smooth %>%
  drop_na(count_whit) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  filter(count_whit == max(count_whit)) %>%
  summarise(
    peak = head(count_whit, 1),
    peak_doy = head(doy_new, 1),
    peak_date_old = head(date, 1)
  )

df_integral <- df_smooth %>%
  drop_na(count_whit) %>%
  mutate(month = format(date, "%m") %>% as.integer()) %>% 
  filter(month %in% 4:10) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  mutate(observ_percent = n() / 214) %>%
  filter(observ_percent >= 0.8) %>% 
  summarise(integral = sum(count_whit) / n() * 214)

df_season <- df_smooth %>%
  drop_na(count_whit) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  filter(count_whit >= quantile(count_whit, 0.3, na.rm = T)) %>%
  summarise(
    sos = min(doy_new),
    eos = max(doy_new)
  ) %>%
  mutate(los = eos - sos)

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
write_rds(df_metrics, str_c(.path$dat_process, "2023-04-25/metrics_offset.rds"))