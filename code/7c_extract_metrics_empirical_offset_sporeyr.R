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
  ) %>% 
  mutate(peak_check = case_when(
    peak_doy %in% 11:355 ~ 1,
    TRUE ~ 0
  )) %>% 
  right_join(df_smooth, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new")) %>% 
  drop_na(peak_doy) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  filter(doy_new %in% (peak_doy - 10):(peak_doy + 10)) %>% 
  mutate(peak_check = case_when(
    sum(is.na(count_whit)) != 0 ~ 0,
    TRUE ~ peak_check)) %>% 
  summarise(
    peak = head(peak, 1),
    peak_doy = head(peak_doy, 1),
    peak_date_old = head(peak_date_old, 1),
    peak_check = head(peak_check, 1)
  )

df_integral <- df_smooth %>%
  drop_na(count_whit) %>%
  mutate(month = format(date, "%m") %>% as.integer()) %>% 
  filter(month %in% 4:10) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  mutate(observ_percent = n() / 214) %>%
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_percent) %>%
  summarise(integral = sum(count_whit) / n() * 214) %>% 
  mutate(integral_check = case_when(
    observ_percent >= 0.8 ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::select(-observ_percent)

df_season <- df_smooth %>%
  drop_na(count_whit) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  mutate(observ_percent = n() / 365) %>%
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_percent) %>%
  filter(count_whit >= quantile(count_whit, 0.3, na.rm = T)) %>%
  summarise(
    sos = min(doy_new),
    eos = max(doy_new),
    sos_date_old = min(date),
    eos_date_old = max(date)
  ) %>%
  mutate(los = eos - sos) %>% 
  mutate(season_check = case_when(
    observ_percent >= 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::select(-observ_percent)

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
write_rds(df_metrics, str_c(.path$dat_process, "2023-04-25/metrics_offset.rds"))