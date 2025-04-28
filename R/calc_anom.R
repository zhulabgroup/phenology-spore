calc_anom <- function(df_raw, metric, pct) {
  df <- df_raw %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(value_anom = Value - mean(Value)) %>%
    mutate(mat_anom = mat - mean(mat)) %>%
    mutate(tap_anom = tap - mean(tap)) %>%
    ungroup()

  return(df)
}
