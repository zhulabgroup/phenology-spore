# fill the dates to full years

tidy_fulldate_station <-function(df_raw) {
  df <- df_raw %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    complete(date = seq(min(year) %>% paste0("-01-01") %>% ymd(), max(year) %>% paste0("-12-31") %>% ymd(), by = "day")) %>%
    mutate(year = format(date, "%Y") %>% as.integer()) %>%
    mutate(doy = format(date, "%j") %>% as.integer()) %>%
    ungroup() %>%
    select(lat, lon, station, city, state, country, id, n, year, doy, date, count)
  
  return(df)
}