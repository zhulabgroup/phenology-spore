calc_lme_climate <- function(df_in, metric, x_lab, pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup()
  
  if (x_lab == "MAT") {
    df <- df %>% 
      rename(climate = mat)
  } else {
    df <- df %>% 
      rename(climate = tap)
  }
  
  tryCatch({
    m_lme <- lme(
      Value ~ climate,
      data = df,
      random = ~ climate | n)
    return(m_lme)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m_lme <- lme(
      Value ~ climate,
      data = df,
      random = ~ climate | n,
      control = lmeControl(opt = "optim"))
    return(m_lme)
  })
}