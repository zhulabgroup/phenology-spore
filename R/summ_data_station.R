#' @export
summ_data_station <- function(df_ts) {
  # sampling period & meta info
  df_station <- df_ts %>%
    group_by(lat, lon, id) %>%
    summarise(`Sampling period` = str_c(min(date), "to", max(date), sep = " "))

  # raw data availability
  df_ava_raw <- df_ts %>%
    mutate(doy = format(date, "%j") %>% as.integer()) %>%
    filter(doy <= 365) %>%
    group_by(city, id, year) %>%
    mutate(nobservation = n()) %>%
    filter(nobservation >= 10) %>%
    ungroup() %>%
    drop_na(count) %>%
    group_by(lat, lon, station, city, state, country, id, n, year) %>%
    summarize(ppt = n() / 365) %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    summarize(ava_raw = mean(ppt)) %>%
    select(lat, lon, id, station, city, state, ava_raw)

  # interpolated data ava
  df_ava_intplt <- df_ts %>%
    mutate(doy = format(date, "%j") %>% as.integer()) %>%
    filter(doy <= 365) %>%
    group_by(city, id, year) %>%
    mutate(nobservation = n()) %>%
    filter(nobservation >= 10) %>%
    ungroup() %>%
    drop_na(count_fillwhit) %>%
    group_by(lat, lon, station, city, state, country, id, n, year) %>%
    summarize(ppt = n() / 365) %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    summarize(ava_intplt = mean(ppt)) %>%
    dplyr::select(lat, lon, id, station, city, state, ava_intplt)

  # combine and calculate the difference (the %interpolated data)
  df_summ_data <- df_ava_raw %>%
    left_join(df_station, by = c("lat", "lon", "id")) %>%
    left_join(df_ava_intplt, by = c("lat", "lon", "id", "station", "city", "state", "country")) %>%
    mutate(diff = ava_intplt - ava_raw) %>%
    mutate(ava_raw = paste0(round(ava_raw * 100, 1), "%")) %>%
    mutate(ava_intplt = paste0(round(ava_intplt * 100, 1), "%")) %>%
    mutate(diff = paste0(round(diff * 100, 1), "%")) %>%
    arrange(city)

  return(df_summ_data)
}
