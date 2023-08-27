pct = 0.6
data_integral_as <- df_metrics %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(integral_as) ~ year_new, .)
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
  right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# fit lme
lme_integral_as <- lme(
  log(integral_as) ~ year_new,
  data = data_integral_as,
  random = ~ year_new | n
)

data_integral_as_climate <- data_integral_as %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>%
  do({
    result <- lm(tap ~ year_new, .)
    data_frame(
      r_squared_tap =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_tap =
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
  rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
  ungroup() %>%
  right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
  do({
    result <- lm(mat ~ year_new, .)
    data_frame(
      r_squared_mat =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_mat =
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
  rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
  ungroup() %>% 
  mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
# fit lm
lm_integral_as <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_integral_as_climate)

summary(lme_integral_as)
summary(lm_integral_as)
