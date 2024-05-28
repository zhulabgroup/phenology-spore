calc_calendar <- function(df_in, df_meta, y_label) {
  df <- df_in %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, doy) %>%
    summarise(count_mean = mean(count, na.rm = T)) %>%
    mutate(count_mean = ifelse(is.nan(count_mean), NA, count_mean)) %>% 
    mutate(count_rescale = ifelse(count_mean < 50000, count_mean, 50000)) %>% 
    mutate(count_rescale = ifelse(count_rescale > 100, count_rescale, 100)) %>% 
    subset(doy != 366) %>% 
    left_join(
      df_meta %>% 
        group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
        summarise(
          tap_mean = mean(tap),
          mat_mean = mean(mat)),
      by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) 
  
  if (y_label == "lon") {
    df_out <- df %>% 
      mutate(ylab = paste0(city, ", ", state, " (",round(-lon), "° W)")) %>% 
      mutate(order = lon)
  } else if (y_label == "ecoregion") {
    df_out <- df %>% 
      filter(n %in% c(10, 9, 13, 22, 6)) %>% 
      mutate(eco = "Mediterranean California") %>% 
      mutate(eco = ifelse(state == "OH", "Eastern Temperate Forests", eco)) %>% 
      mutate(eco = ifelse(state == "MO", "Eastern Temperate Forests", eco)) %>% 
      mutate(eco = ifelse(state == "OK", "Great Plains", eco)) %>% 
      mutate(ylab = paste0(city, ", ", state, "\n(",eco, ")")) %>% 
      mutate(order = lon)
  } else if (y_label == "mm") {
    df_out <- df %>% 
      filter(n %in% c(5, 44, 7, 38, 52)) %>% 
      mutate(ylab = paste0(city, ", ", state, "\n(", round(tap_mean), " mm)")) %>% 
      mutate(order = tap_mean)
  }
  
  return(df_out)
}