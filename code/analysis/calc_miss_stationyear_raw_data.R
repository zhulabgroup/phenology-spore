# check the proportion of missing data in each station-year
# for leap year, we remove the 366 doy

calc_miss_stationyear_raw_data <- function(df_raw) {
  df <- df_raw %>% 
    mutate(doy = format(date, "%j") %>% as.integer()) %>%
    filter(doy <= 365) %>%
    drop_na(count) %>% 
    group_by(lat, lon, station, city, state, country, id, year) %>%
    summarize(miss_ppt = 1 - n() / 365)
  
  return(df)
}