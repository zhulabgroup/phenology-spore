#' @export
calc_lme_all <- function(df_analysis, pct = 0.8) {
  # generate the dataframe
  df_m <- data.frame()
  for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    for (x in c("year_new", "MAT", "TAP")) {
      m_rslt <- calc_lme(df_in = df_analysis, metric = m_metric, x_vrb = x, pct = pct)
      df_m <- rbind(df_m, m_rslt)
    }
  }
  colnames(df_m) <- c("metric", "cpltness", "n_obsv", "change", "x_variable", "beta", "ci1", "ci2", "p")
  df <- df_m %>%
    mutate(beta = as.numeric(beta)) %>%
    mutate(ci1 = as.numeric(ci1)) %>%
    mutate(ci2 = as.numeric(ci2)) %>%
    mutate(p = as.numeric(p)) %>%
    mutate(pspct = ifelse(
      metric %in% c("SOS", "EOS", "LOS", "ln_Ca", "ln_AIn"),
      "Ecology Perspective",
      "Public Health Perspective"
    )) %>%
    mutate(metric = case_when(
      metric == "ln_Ca" ~ "ln(Ca)",
      metric == "ln_Cp" ~ "ln(Cp)",
      metric == "ln_AIn" ~ "ln(AIn)",
      metric == "ln_ASIn" ~ "ln(ASIn)",
      T ~ metric
    )) %>%
    mutate(x_variable = ifelse(x_variable == "year_new", "year", x_variable)) %>%
    mutate(mtype = ifelse(
      metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS"),
      "pheno",
      "intst"
    )) %>%
    mutate(transparency = ifelse(p < 0.05, 1, 0.5))

  return(df)
}

#' @export
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

  m_lme <- tryCatch(
    {
      nlme::lme(
        Value ~ x_variable,
        data = df,
        random = ~ x_variable | n
      )
    },
    error = function(e) {
      nlme::lme(
        Value ~ x_variable,
        data = df,
        random = ~ x_variable | n,
        control = nlme::lmeControl(opt = "optim")
      )
    }
  )

  # Now process the fitted model once
  n_obsv <- nobs(m_lme)
  beta <- nlme::fixef(m_lme)[["x_variable"]] %>%
    as.numeric() %>%
    round(5)
  alpha <- nlme::fixef(m_lme)[["(Intercept)"]] %>%
    as.numeric() %>%
    round(5)
  y2003 <- alpha + beta * 2003
  y2022 <- alpha + beta * 2022

  if (metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS")) {
    change <- (y2022 - y2003) %>%
      as.numeric() %>%
      round(5)
  } else {
    change <- (((exp(y2022) - 1) - (exp(y2003) - 1)) / (exp(y2003) - 1)) %>%
      as.numeric() %>%
      round(5)
  }

  CI <- nlme::intervals(m_lme, which = "fixed")
  CI1 <- CI$fixed[2, "lower"] %>%
    as.numeric() %>%
    round(5)
  CI2 <- CI$fixed[2, "upper"] %>%
    as.numeric() %>%
    round(5)
  p <- summary(m_lme)$tTable[["x_variable", "p-value"]] %>%
    as.numeric() %>%
    round(5)

  # Final result
  result <- c(metric, pct, n_obsv, change, x_vrb, beta, CI1, CI2, p)
  return(result)
}
