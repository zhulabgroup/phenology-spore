df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df <- df_smooth %>% 
  drop_na(count_whit) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  mutate(observ_pct = n() / 365) %>%
  ungroup()

df_peak <- df %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>%
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
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>%
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

df_integral <- df %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>%
  summarise(integral = sum(count_whit) / n() * 365)

df_season <- df %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>%
  filter(cumsum(count_whit) >= 0.1 * sum(count_whit) & cumsum(count_whit) <= 0.9 * sum(count_whit)) %>%
  summarise(
    sos = min(doy_new),
    eos = max(doy_new),
    sos_date_old = min(date),
    eos_date_old = max(date)
  ) %>%
  mutate(los = eos - sos)

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "observ_pct"))
write_rds(df_metrics, str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))