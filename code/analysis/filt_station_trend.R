source("~/Github/spore_phenology/code/analysis/summ_ecoregion_station.R")

filt_station_trend <- function(df_in) {
  df_ecorg <- summ_ecoregion_station(df_in) %>% 
    dplyr::select(-lon, -lat) %>% 
    right_join(
      df_in,
      by = c("station", "city", "state", "country", "id", "n", "offset", "r_squared", "p_value", "intercept", "slope", "year_new", "mat", "tap", "cpltness", "Metric", "Value", "Nyear", "rescaled_slope", "col"))
  
  df <- df_ecorg %>% 
    distinct(n, .keep_all = T) %>% 
    arrange(ecoregion, -Nyear) %>% 
    group_by(ecoregion) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(lon)
  
  return(df)
}