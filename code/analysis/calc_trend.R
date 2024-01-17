# # tidy up metrics data
# df_tag <- df_metrics %>%
#   gather(key = "Metric", value = "Value", peak, ln_peak, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>%
#   mutate(cpltness = ifelse(Metric %in% c("sas", "eas", "las"), 1, cpltness)) %>%
#   mutate(cpltness = ifelse(Metric %in% c("integral_as", "ln_integral_as"), cpltness_as, cpltness)) %>%
#   dplyr::select(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, Metric, Value) %>%
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

# # fit lme to calculate trend (>= 5 years)
# calc_trend_metric <- function(df_raw, metric, pct) {
#   df <- df_raw %>%
#     filter(Metric == metric) %>%
#     filter(cpltness >= pct) %>%
#     drop_na(Value) %>%
#     group_by(lat, lon, station, city, state, country, id, n, offset) %>%
#     filter(n() >= 5) %>%
#     ungroup()
# 
#   m <- lme(
#     Value ~ year_new,
#     data = df,
#     random = ~ year_new | n
#   )
# 
#   beta <- fixef(m)[["year_new"]] %>% round(3)
#   p <- summary(m)$tTable[["year_new", "p-value"]] %>% round(3)
# 
#   result <- list(metric, beta, p)
# 
#   return(result)
# }

calc_trend_metric <- function(df_raw, metric, pct) {
    df <- df_raw %>%
      filter(Metric == metric) %>%
      filter(cpltness >= pct) %>%
      drop_na(Value) %>%
      group_by(lat, lon, station, city, state, country, id, n, offset) %>%
      filter(n() >= 5) %>%
      ungroup()

  tryCatch({
    # Fit linear mixed-effects model
    m <- lme(
      Value ~ year_new,
      data = df,
      random = ~ year_new | n
    )

    # Extract beta and p-value
    beta <- fixef(m)[["year_new"]] %>% round(3)
    p <- summary(m)$tTable[["year_new", "p-value"]] %>% round(3)

    # Create a list with metric name, beta, and p-value
    result <- list(metric, beta, p)

    return(result)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")

    # Retry with the control parameter
    m <- lme(
      Value ~ year_new,
      data = df,
      random = ~ year_new | n,
      control = lmeControl(opt = "optim")
    )

    # Extract beta and p-value
    beta <- fixef(m)[["year_new"]] %>% round(3)
    p <- summary(m)$tTable[["year_new", "p-value"]] %>% round(3)

    # Create a list with metric name, beta, and p-value
    result <- list(metric, beta, p)

    return(result)
  })
}