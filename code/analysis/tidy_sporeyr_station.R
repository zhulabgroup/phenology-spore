# define spore year for each station

tidy_sporeyr_station <- function(df_raw) {
  df <- df_raw %>% 
    group_by(lat, lon, station, city, state, country, id, n) %>%
    mutate(date = as.Date(date)) %>% 
    mutate(date_new = case_when(offset < 183 ~ date - offset,
                                T ~ date +365 - offset)) %>% 
    mutate(year_new = format(date_new, "%Y") %>% as.numeric()) %>%
    mutate(doy_new = format(date_new, "%j") %>% as.numeric()) %>%
    ungroup()

  return(df)
}