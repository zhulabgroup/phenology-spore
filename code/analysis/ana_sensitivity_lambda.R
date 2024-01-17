source("tidy_smoothfillwhit_station.R")
source("tidy_sporeyr_station.R")
source("calc_completeness_stationspyr.R")
source("calc_metrics_stationspyr.R")
source("calc_trend.R")
source("ana_sensitivity_lambda.R")

sen <- function(df_in, lmd) {
  df_smooth_lmd <- df_in %>%
    tidy_smoothfillwhit_station(n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit)
  
  df_sporeyr = df_smooth_lmd %>% 
    tidy_sporeyr_station()
  
  df_spore_cpltness <- calc_completeness_stationspyr(df_sporeyr, column_name = count_fillwhit)
  
  df_metrics <- calc_metrics_stationspyr(df_completeness = df_spore_cpltness, df_raw = df_sporeyr)
  df_tag <- df_metrics %>%
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
    mutate(Metric = ifelse(Metric == "amplitude", "A", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral", "AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral_as", "ASIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_peak", "ln_Cp", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_amplitude", "ln_A", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral", "ln_AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral_as", "ln_ASIn", Metric))
  
  m_Cp <- calc_trend_metric(df_raw = df_tag, metric = "ln_Cp", pct = 0.8)
  m_AIn <- calc_trend_metric(df_raw = df_tag, metric = "ln_AIn", pct = 0.8)
  m_ASIn <- calc_trend_metric(df_raw = df_tag, metric = "ln_ASIn", pct = 0.8)
  m_SOS <- calc_trend_metric(df_raw = df_tag, metric = "SOS", pct = 0.8)
  m_EOS <- calc_trend_metric(df_raw = df_tag, metric = "EOS", pct = 0.8)
  m_LOS <- calc_trend_metric(df_raw = df_tag, metric = "LOS", pct = 0.8)
  m_SAS <- calc_trend_metric(df_raw = df_tag, metric = "SAS", pct = 0.8)
  m_EAS <- calc_trend_metric(df_raw = df_tag, metric = "EAS", pct = 0.8)
  m_LAS <- calc_trend_metric(df_raw = df_tag, metric = "LAS", pct = 0.8)
  m_A <- calc_trend_metric(df_raw = df_tag, metric = "ln_A", pct = 0.8)
  df_output <- rbind(m_SOS, m_EOS, m_LOS, m_SAS, m_EAS, m_LAS, m_Cp, m_A, m_AIn, m_ASIn) %>% 
    as.data.frame() %>% 
    rename(
      metric = V1,
      beta = V2,
      p = V3
    )
  rownames(df_output) <- NULL
  
  return(df_output)
}

l = 2100
df_2100 <- sen(df_wavelet, lmd = l)
df_2100
