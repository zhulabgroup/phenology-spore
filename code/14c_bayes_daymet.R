meta <- meta_df %>%
  filter(location == df_pheno_model$location %>% unique())
df_daymet <- daymetr::download_daymet(
  site = meta$location,
  lat = meta$lat,
  lon = meta$lon,
  start = lubridate::year(min(df_pheno_model$date)),
  end = min(lubridate::year(max(df_pheno_model$date)) + 1, 2020),
  internal = TRUE,
  simplify = TRUE
) %>%
  filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.", "vp..Pa.")) %>%
  spread(key = "measurement", value = "value") %>%
  rename(
    prcp = `prcp..mm.day.`,
    tmax = `tmax..deg.c.`,
    tmin = `tmin..deg.c.`,
    vp = `vp..Pa.`
  ) %>%
  mutate(date = as.Date(yday, origin = paste0(year, "-01-01")) - 1) %>%
  mutate(temp = (tmax + tmin / 2)) %>%
  select(date, temp, prcp, vp) %>%
  mutate(year = lubridate::year(date))

df_daymet_annual <- df_daymet %>%
  filter(year %in% (df_pheno_model %>% pull(year) %>% unique())) %>%
  group_by(year) %>%
  summarise(
    mat = mean(temp, na.rm = T),
    tap = sum(prcp, na.rm = T),
    mvp = mean(vp, na.rm = T),
  ) %>%
  ungroup()

df_pheno_model <- df_pheno_model %>%
  left_join(df_daymet_annual, by = c("year")) %>%
  drop_na(tap, mat, mvp)
