# check the proportion of missing data in each station-year
# for leap year, we remove the 366 doy

summ_miss_stationyear_raw_data <- function(df_raw, column_name) {
  df <- df_raw %>%
    mutate(doy = format(date, "%j") %>% as.integer()) %>%
    filter(doy <= 365) %>%
    group_by(city, id, year) %>%
    mutate(nobservation = n()) %>%
    filter(nobservation >= 10) %>%
    ungroup() %>%
    drop_na({{ column_name }}) %>%
    group_by(lat, lon, station, city, state, country, id, n, year) %>%
    summarize(miss_ppt = 1 - n() / 365) %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    summarize(miss = mean(miss_ppt))

  return(df)
}
