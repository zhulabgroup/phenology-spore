## peak
# filter data
i = 4
pct = 1 - 0.1*(i - 1)
data_peak <- df_metrics %>% 
  filter(peak_check == 1) %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(peak_doy) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(peak_doy ~ year_new, .)
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
  filter(peak_check == 1) %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(peak_doy) %>% 
  filter(observ_pct >= pct) %>% 
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
  filter(peak_check == 1) %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(peak_doy) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
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
  ungroup()
lme_climate_temporal <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_peak)
summary(lme_climate_temporal)

## integral
# filter data
i = 4
pct = 1 - 0.1*(i - 1)
data_integral <- df_metrics %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(integral) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(integral) ~ year_new, .)
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
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(integral) %>%
  filter(observ_pct >= pct) %>%
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
  drop_na(integral) %>%
  filter(observ_pct >= pct) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
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
  ungroup()
lme_climate_temporal <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_integral)
summary(lme_climate_temporal)

## season
# filter data
i = 1
pct = 1 - 0.1*(i - 1)
data_season <- df_metrics %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(sos ~ year_new, .)
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
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= pct) %>% 
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
  drop_na(sos) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
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
  ungroup()
lm_climate_temporal <- lm(
  slope ~ 1 + slope_tap,
  data = data_season)
summary(lm_climate_temporal)

## as season
i = 1
pct = 1 - 0.1*(i - 1)
data_integral_as <- df_metrics %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(las ~ year_new, .)
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
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= pct) %>% 
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
  drop_na(las) %>% 
  filter(observ_pct_as >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
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
  ungroup() 

lme_climate_temporal <- lm(
  slope ~ slope_mat + slope_tap,
  data = data_integral_as)
summary(lme_climate_temporal)

## sas eas
data_sas <- df_metrics %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(eas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(eas ~ year_new, .)
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
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  drop_na(eas) %>% 
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
  drop_na(eas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
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
  ungroup() 

lme_climate_temporal <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_sas)
summary(lme_climate_temporal)
