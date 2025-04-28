# only include stations in continental US
# zoom in to a specific family f
# include stations with measurements >= n_m in each year
# and years >= n_yr in each station

filt_taxa_station_raw_data <- function(df_spore, f, n_m, n_yr) {
  df <- df_spore %>%
    filter(family == f) %>%
    filter(country == "US") %>%
    filter(!(state %in% c("PR", "AK", "HI"))) %>%
    mutate(count = abs(count)) %>%
    filter(count > 0) %>%
    mutate(year = format(date, "%Y") %>% as.integer()) %>%
    group_by(city, id, year) %>%
    mutate(nobservation = n()) %>%
    filter(nobservation >= n_m) %>%
    ungroup() %>%
    group_by(city, id) %>%
    mutate(nyear = length(unique(year))) %>%
    filter(nyear >= n_yr) %>%
    ungroup() %>%
    dplyr::select(lat, lon, station, city, state, country, id, year, date, count)

  return(df)
}
