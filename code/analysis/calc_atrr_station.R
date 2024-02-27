calc_atrr_station <- function(df_in, metric, cli_vrb, pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup()
  if (cli_vrb == "MAT") {
    df <- df %>% 
      rename(climate = mat)
  } else {
    df <- df %>% 
      rename(climate = tap)
  }
  
  df_lm <- df %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    do({
      result <- lm(Value ~ climate, .)
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
    rename("slope" = "climate", "intercept" = "(Intercept)") %>%
    ungroup() %>%
    right_join(df, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset"))
  
  return(df_lm)
}

# df_atrr_station <- calc_atrr_station(df_in = df_ana, metric = "SOS", cli_vrb = "MAT", pct = 0.8)