calc_trend_station <- function(df_in, metric, pct) {
  #filter station-year
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup()
  # fit lm for each station
  df_lm <- df %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    do({
      result <- lm(Value ~ year_new, .)
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
    rename("slope" = "year_new", "intercept" = "(Intercept)") %>%
    ungroup() %>%
    right_join(df, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
    mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
  
  return(df_lm)
}
