filt_station_NofYears_metric <- function(df_in, pct) {
  df_out <- data.frame()
  for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "Ca", "ln_Ca", "Cp", "ln_Cp", "AIn", "ln_AIn", "ASIn", "ln_ASIn")) {
    df <- df_in %>%
      filter(Metric == m_metric) %>%
      filter(cpltness >= pct) %>%
      drop_na(Value) %>%
      group_by(lat, lon, station, city, state, country, id, n, offset) %>%
      filter(n() >= 5) %>%
      mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
      ungroup()
    df_out <-rbind(df_out, df)
  }
  
  return(df_out)
}