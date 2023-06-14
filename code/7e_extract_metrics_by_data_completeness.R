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

find_sas <- function(x) {
  # find the first day of 10 consecutive days with spore count >= 6500
  # 10 values before sas are not NA, otherwise, return NA
  # if sas <= 10, all the values before sas should not be NA, otherwise return NA
  # reverse order for eas
  consecutive_count <- 0
  you <- NA
  you_date_old <- NA
  for (i in 1:nrow(x)) {
    if (!is.na(x$count_whit[i]) && x$count_whit[i] >= 6500) {
      consecutive_count <- consecutive_count + 1
        if (consecutive_count == 1) {
          you <- x$doy_new[i]
          you_date_old <- x$date[i]
        }
        if (consecutive_count == 10) {
          break
        }
    } else {
      consecutive_count <- 0
    }
  }
  if (!is.na(you) && i > 10 && any(is.na(x$count_whit[(i - 10):i]))) {
    you <- NA
    you_date_old <- NA
  } else if (!is.na(you) && i <= 10 && any(is.na(x$count_whit[1:i]))) {
    you <- NA
    you_date_old <- NA
  }
  x$you <- you
  x$you_date_old <- you_date_old
  return(x)
}

df_allergy_season <- df_smooth %>% 
  drop_na(count_whit) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
  summarise(observ_pct = n() / 365) %>%
  right_join(df_smooth, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new")) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>% 
  group_modify(~ find_sas(.)) %>% 
  ungroup() %>% 
  rename(
    sas = you,
    sas_date_old = you_date_old
    ) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>% 
  arrange(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct, desc(doy_new)) %>% 
  group_modify(~ find_sas(.)) %>% 
  arrange(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct, doy_new) %>% 
  ungroup() %>% 
  rename(
    eas = you,
    eas_date_old = you_date_old
  ) %>% 
  mutate(las = eas - sas + 1) %>%
  drop_na(count_whit) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>%
  summarise(
    sas = head(sas, 1),
    eas = head(eas, 1),
    las = head(las, 1),
    sas_date_old = head(sas_date_old, 1),
    eas_date_old = head(eas_date_old, 1)
  )

df_integral_as <- df_allergy_season %>% 
  right_join(df, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "observ_pct")) %>% 
  filter(!is.na(las)) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct, sas, eas, las, sas_date_old, eas_date_old) %>% 
  filter(doy_new %in% sas:eas) %>% 
  summarise(
    observ_pct_as = n() / las,
    integral_as = sum(count_whit) / n() * las
    ) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new, observ_pct) %>% 
  summarise(
    observ_pct_as = head(observ_pct_as, 1),
    integral_as = head(integral_as, 1)
    )

df_metrics <- list(
  df_peak,
  df_integral,
  df_season,
  df_allergy_season,
  df_integral_as
  ) %>% 
  reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "observ_pct"))
write_rds(df_metrics, str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))
