# df_tag <- df_analysis %>%
#   gather(key = "Metric", value = "Value", peak, ln_peak, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>%
#   mutate(cpltness = ifelse(Metric %in% c("sas", "eas", "las"), 1, cpltness)) %>%
#   mutate(cpltness = ifelse(Metric %in% c("integral_as", "ln_integral_as"), cpltness_as, cpltness)) %>%
#   dplyr::select(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, mat, tap, Metric, Value) %>%
#   mutate(Metric = ifelse(Metric == "sos", "SOS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "eos", "EOS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "los", "LOS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "sas", "SAS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "eas", "EAS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "las", "LAS", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "peak", "Cp", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "amplitude", "A", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "integral", "AIn", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "integral_as", "ASIn", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "ln_peak", "ln_Cp", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "ln_amplitude", "ln_A", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "ln_integral", "ln_AIn", Metric)) %>%
#   mutate(Metric = ifelse(Metric == "ln_integral_as", "ln_ASIn", Metric))

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
  
  m <- lm(
    value_anom ~ 1 + mat_anom + tap_anom + mat_anom * tap_anom,
    data = df)
  
  return(summary(m))
}