calc_lm_anom <- function(df_in, metric, pct, x_lab) {
  
  df_anom <- calc_anom(df_raw = df_in, metric = metric, pct = pct)
  
  if (x_lab == "MAT_anom") {
    df_anom <- rename(df_anom, climate_anom = mat_anom)
  } else {
    df_anom <- rename(df_anom, climate_anom = tap_anom)
  }
  
  df_lm <- df_anom %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    do({
      result <- lm(value_anom ~ climate_anom, .)
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
    rename("slope" = "climate_anom", "intercept" = "(Intercept)") %>%
    ungroup() %>%
    right_join(df_anom, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset"))
  
  return(df_lm)
}