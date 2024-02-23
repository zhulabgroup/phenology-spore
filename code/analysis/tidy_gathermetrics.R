# gather df_metrics into a long df and name the metrics
tidy_gathermetrics <- function(df_in) {
  df <- df_in %>%
    gather(key = "Metric", value = "Value", peak, ln_peak, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>%
    mutate(cpltness = ifelse(Metric %in% c("sas", "eas", "las"), 1, cpltness)) %>%
    mutate(cpltness = ifelse(Metric %in% c("integral_as", "ln_integral_as"), cpltness_as, cpltness)) %>%
    dplyr::select(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, Metric, Value) %>%
    mutate(Metric = ifelse(Metric == "sos", "SOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "eos", "EOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "los", "LOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "sas", "SAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "eas", "EAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "las", "LAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "peak", "Cp", Metric)) %>%
    mutate(Metric = ifelse(Metric == "amplitude", "Ca", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral", "AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral_as", "ASIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_peak", "ln_Cp", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_amplitude", "ln_Ca", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral", "ln_AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral_as", "ln_ASIn", Metric))
  
  return(df)
}