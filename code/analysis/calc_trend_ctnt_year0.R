calc_trend_station_year0 <- function(df_in, metric, pct) {
  
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year0) - min(year0) + 1) %>% 
    ungroup()
  
  df_lm <- df %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    do({
      result <- lm(Value ~ year0, .)
      data_frame(
        r_squared =
          result %>%
          summary() %>%
          magrittr::use_series(adj.r.squared),
        p_value =
          result %>%
          anova() %>%
          magrittr::use_series(`Pr(>F)`) %>%
          magrittr::extract2(1)
      ) %>%
        bind_cols(
          result %>%
            coef() %>%
            as.list() %>%
            as_data_frame()
        )
    }) %>%
    rename("slope" = "year0", "intercept" = "(Intercept)") %>%
    ungroup() %>%
    right_join(df, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset"))
  
  return(df_lm)
}

calc_trend_ctnt_year0 <- function(df_in) {
  tryCatch({
    m_ctnt <- lme(
      Value ~ year0,
      data = df_in,
      random = ~ year0 | n)
    return(m_ctnt)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m_ctnt <- lme(
      Value ~ year0,
      data = df_in,
      random = ~ year0 | n,
      control = lmeControl(opt = "optim"))
    return(m_ctnt)
  })
}

df_ana_year0 <- df_ana %>% 
  mutate(year0 = year_new - 2003)
c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")
m = "ln_ASIn"
df_trend_station_year0 <- calc_trend_station_year0(df_in = df_ana_year0, metric = m, pct = 0.8)
m_trend_year0 <- calc_trend_ctnt_year0(df_trend_station_year0)
summary(m_trend_year0)