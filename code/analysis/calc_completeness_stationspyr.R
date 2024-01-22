# calculate data completeness in each station-sporeyr for future quality control

calc_completeness_stationspyr <- function(df_raw, column_name) {
  df <- df_raw %>% 
    drop_na({{column_name}}) %>%
    group_by(lat, lon, station, city, state, country, id, n, year_new) %>%
    mutate(cpltness = n() / 365) %>%
    ungroup()
  
  return(df)
}

