#' @export
tidy_trend_metric <- function(df_lme) {
  df_summ_metric_new <- df_lme %>%
    filter(x_variable == "year") %>%
    mutate(metric = case_when(
      metric == "ln_Ca" ~ "ln(Ca)",
      metric == "ln_Cp" ~ "ln(Cp)",
      metric == "ln_AIn" ~ "ln(AIn)",
      metric == "ln_ASIn" ~ "ln(ASIn)",
      T ~ metric
    )) %>%
    mutate(
      change = as.numeric(change),
      n_station = as.integer(n_station)
    ) %>%
    mutate(change_latent = ifelse(
      metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS"),
      str_c(change %>% round(2), "d"),
      str_c((change * 100) %>% round(2), "%")
    )) %>%
    mutate(change = ifelse(
      change > 0,
      paste0("+", change_latent),
      change_latent
    )) %>%
    mutate(beta = beta %>% round(4) %>% as.character()) %>%
    mutate(p = p %>% round(4) %>% as.character()) %>%
    select(
      "Metric" = "metric",
      "Number of stations" = "n_station",
      "Number of observations" = "n_obsv",
      "Slope" = "beta",
      "Change" = "change",
      "p-value" = "p"
    )

  return(df_summ_metric_new)
}
