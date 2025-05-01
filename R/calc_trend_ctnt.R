#' @export
calc_trend_ctnt <- function(df_in, metric, x_var = "year_new", pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(x = !!sym(x_var))

  tryCatch(
    {
      m_ctnt <- nlme::lme(
        Value ~ x,
        data = df,
        random = ~ x | n
      )
      return(m_ctnt)
    },
    error = function(e) {
      # cat("Error in lme formula:", conditionMessage(e), "\n")
      # Retry with the control parameter
      m_ctnt <- nlme::lme(
        Value ~ x,
        data = df,
        random = ~ x | n,
        control = nlme::lmeControl(opt = "optim")
      )
      return(m_ctnt)
    }
  )
}
