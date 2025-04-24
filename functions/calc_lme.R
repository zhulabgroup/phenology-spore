calc_lme <- function(df_in, metric, x_vrb, pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup()
  
  if (x_vrb == "MAT") {
    df <- df %>% 
      rename(x_variable = mat)
  }
  
  if (x_vrb == "TAP") {
    df <- df %>% 
      rename(x_variable = tap)
  }
  
  if (x_vrb == "year_new") {
    df <- df %>% 
      rename(x_variable = year_new)
  }
  
  tryCatch({
    m_lme <- nlme::lme(
      Value ~ x_variable,
      data = df,
      random = ~ x_variable | n) 
    # Extract CI
    n_obsv <- nobs(m_lme)
    beta <- lme4::fixef(m_lme)[["x_variable"]] %>% as.numeric() %>% round(5)
    alpha <- lme4::fixef(m_lme)[["(Intercept)"]] %>% as.numeric() %>% round(5)
    y2003 <- alpha + beta * 2003
    y2022 <- alpha + beta * 2022
    if (metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS")) {
      change <- y2022 - y2003
    } else {
      change <- ((exp(y2022) - 1) - (exp(y2003) - 1)) / (exp(y2003) - 1)
    }
    CI <- nlme::intervals(m_lme, which = "fixed")
    CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% round(5)
    CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% round(5)
    p <- summary(m_lme)$tTable[["x_variable", "p-value"]] %>% as.numeric() %>% round(5)
    # Create a list with metric name, ci
    result <- c(metric, pct, n_obsv, change, x_vrb, beta, CI1, CI2, p)
    return(result)
  }, error = function(e) {
    # cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m_lme <- nlme::lme(
      Value ~ x_variable,
      data = df,
      random = ~ x_variable | n,
      control = nlme::lmeControl(opt = "optim"))
    # Extract CI
    n_obsv <- nobs(m_lme)
    beta <- lme4::fixef(m_lme)[["x_variable"]] %>% as.numeric() %>% round(5)
    alpha <- lme4::fixef(m_lme)[["(Intercept)"]] %>% as.numeric() %>% round(5)
    y2003 <- alpha + beta * 2003
    y2022 <- alpha + beta * 2022
    if (metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS")) {
      change <- (y2022 - y2003) %>% as.numeric() %>% round(5)
    } else {
      change <- (((exp(y2022) - 1) - (exp(y2003) - 1)) / (exp(y2003) - 1)) %>% as.numeric() %>% round(5)
    }
    CI <- nlme::intervals(m_lme, which = "fixed")
    CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% round(5)
    CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% round(5)
    p <- summary(m_lme)$tTable[["x_variable", "p-value"]] %>% as.numeric() %>% round(5)
    # Create a list with metric name, ci
    result <- c(metric, pct, n_obsv, change, x_vrb, beta, CI1, CI2, p)
    return(result)
  })
}