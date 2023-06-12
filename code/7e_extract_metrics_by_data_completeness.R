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
  # and ensure 10 values before sas are not NA
  # reverse order for eas
  consecutive_count <- 0
  you <- NA
  you_date_old <- NA
  for (i in 1:nrow(x)) {
    if (!is.na(x$count_whit[i]) && x$count_whit[i] >= 6500) {
      if (i > 10 && all(!is.na(x$count_whit[(i - 10):i]))) {
        consecutive_count <- consecutive_count + 1
        if (consecutive_count == 1) {
          you <- x$doy_new[i]
          you_date_old <- x$date[i]
        }
        if (consecutive_count == 10) {
          break
        }
      } else if (i <= 10 && all(!is.na(x$count_whit[1:i]))) {
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
    } else {
      consecutive_count <- 0
    }
  }
  x$you <- you
  x$you_date_old <- you_date_old
  return(x)
}

view(df_allergy_season %>% distinct(id, year_new, .keep_all = T) %>% drop_na(eas))

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
  mutate(las = )

df_metrics <- list(df_peak, df_integral, df_season) %>% reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "observ_pct"))
write_rds(df_metrics, str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))