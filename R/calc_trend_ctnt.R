#' @export
calc_trend_ctnt <- function(df_in, metric, pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>%
    ungroup()

  tryCatch(
    {
      m_ctnt <- nlme::lme(
        Value ~ year_new,
        data = df,
        random = ~ year_new | n
      )
      return(m_ctnt)
    },
    error = function(e) {
      # cat("Error in lme formula:", conditionMessage(e), "\n")
      # Retry with the control parameter
      m_ctnt <- nlme::lme(
        Value ~ year_new,
        data = df,
        random = ~ year_new | n,
        control = nlme::lmeControl(opt = "optim")
      )
      return(m_ctnt)
    }
  )
}
