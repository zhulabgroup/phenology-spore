# fit lme to calculate trend (>= 5 years)
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
    beta <- fixef(m)[["year_new"]] %>% as.numeric() %>% round(5)
    CI <- intervals(m, which = "fixed")
    CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% round(5)
    CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% round(5)
    p <- summary(m)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(5)

    # Create a list with metric name, beta, and p-value
    result <- c(metric, beta, CI1, CI2, p)

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

    # Extract beta, CI, and p-value
    beta <- fixef(m)[["year_new"]] %>% as.numeric() %>% round(5)
    CI <- intervals(m, which = "fixed")
    CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% round(5)
    CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% round(5)
    p <- summary(m)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(5)

    # Create a list with metric name, beta, and p-value
    result <- c(metric, beta, CI1, CI2, p)

    return(result)
  })
}
