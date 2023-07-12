# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_daymet.rds"))
# pct = 1

## peak
# filter data
data_peak <- df_metrics %>% 
  filter(peak_check == 1) %>%
  drop_na(peak) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(peak) ~ year_new, .)
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
  drop_na(peak) %>% 
  filter(observ_pct >= pct) %>% 
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
lme_peak <- lme(
  log(peak) ~ year_new,
  data = data_peak,
  random = ~ year_new | n
)
fixed_effects_peak_lme <- fixef(lme_peak)
peak_fit_lme <- data_peak %>% 
  mutate(
    lme.fixed = lme_peak$fitted[, 1],
    lme.random = lme_peak$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_peak_lme <- ggpredict(lme_peak, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_peak <- ggplot() +
  geom_point(
    data = peak_fit_lme,
    aes(x = year_new, y = log(peak), col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = peak_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = log(peak), group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_peak_lme,
    aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = peak_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Peak concentration (grains / m^3)") +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  labs(title = expression(paste(bold("A      "), italic("Decreased Peak Concentration")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
    )



## annual integral
# filter data
data_integral <- df_metrics %>% 
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
  drop_na(integral) %>% 
  filter(observ_pct >= pct) %>% 
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
lme_integral <- lme(
  log(integral) ~ year_new,
  data = data_integral,
  random = ~ year_new | n
)
fixed_effects_integral_lme <- fixef(lme_integral)
integral_fit_lme <- data_integral %>% 
  mutate(
    lme.fixed = lme_integral$fitted[, 1],
    lme.random = lme_integral$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_integral_lme <- ggpredict(lme_integral, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_integral <- ggplot() +
  geom_point(
    data = integral_fit_lme,
    aes(x = year_new, y = log(integral), col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = integral_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = log(integral), group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_integral_lme,
    aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = integral_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Annual spore integral (grains / m^3)") +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  labs(title = expression(paste(bold("B      "), italic("Decreased Annual Integral")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )





## as integral
# filter data
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
fixed_effects_integral_as_lme <- fixef(lme_integral_as)
integral_as_fit_lme <- data_integral_as %>% 
  mutate(
    lme.fixed = lme_integral_as$fitted[, 1],
    lme.random = lme_integral_as$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_integral_as_lme <- ggpredict(lme_integral_as, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_integral_as <- ggplot() +
  geom_point(
    data = integral_as_fit_lme,
    aes(x = year_new, y = log(integral_as), col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = integral_as_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = log(integral_as), group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_integral_as_lme,
    aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = integral_as_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Allergy season spore integral (grains / m^3)") +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  labs(title = expression(paste(bold("C      "), italic("Increased Allergy Season Integral")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )





## sos
# filter data
data_sos <- df_metrics %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= 1) %>% 
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
  drop_na(sos) %>% 
  filter(observ_pct >= 1) %>% 
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
lme_sos <- lme(
  sos ~ year_new,
  data = data_sos,
  random = ~ year_new | n
)
fixed_effects_sos_lme <- fixef(lme_sos)
sos_fit_lme <- data_sos %>% 
  mutate(
    lme.fixed = lme_sos$fitted[, 1],
    lme.random = lme_sos$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_sos_lme <- ggpredict(lme_sos, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_sos <- ggplot() +
  geom_point(
    data = sos_fit_lme,
    aes(x = year_new, y = sos, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = sos_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = sos, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_sos_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = sos_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Sos (day of year)") +
  labs(title = expression(paste(bold("D      "), italic("Advanced Start of Spore Season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )




## los
# filter data
data_los <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(los ~ year_new, .)
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
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>% 
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
lme_los <- lme(
  los ~ year_new,
  data = data_los,
  random = ~ year_new | n
)
fixed_effects_los_lme <- fixef(lme_los)
los_fit_lme <- data_los %>% 
  mutate(
    lme.fixed = lme_los$fitted[, 1],
    lme.random = lme_los$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_los_lme <- ggpredict(lme_los, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_los <- ggplot() +
  geom_point(
    data = los_fit_lme,
    aes(x = year_new, y = los, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = los_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = los, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_los_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = los_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Los (days)") +
  labs(title = expression(paste(bold("F      "), italic("Extended Length of Spore Season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )
  



## sas
# filter data
data_sas <- df_metrics %>% 
  drop_na(sas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(sas ~ year_new, .)
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
  drop_na(sas) %>% 
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
lme_sas <- lme(
  sas ~ year_new,
  data = data_sas,
  random = ~ year_new | n
)
fixed_effects_sas_lme <- fixef(lme_sas)
sas_fit_lme <- data_sas %>% 
  mutate(
    lme.fixed = lme_sas$fitted[, 1],
    lme.random = lme_sas$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_sas_lme <- ggpredict(lme_sas, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_sas <- ggplot() +
  geom_point(
    data = sas_fit_lme,
    aes(x = year_new, y = sas, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = sas_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = sas, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_sas_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = sas_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Sas (day of year)") +
  ylab("Los (days)") +
  labs(title = expression(paste(bold("E      "), italic("Advanced Start of Allergy Season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )




## las
# filter data
data_las <- df_metrics %>% 
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
  drop_na(las) %>% 
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
lme_las <- lme(
  las ~ year_new,
  data = data_las,
  random = ~ year_new | n
)
fixed_effects_las_lme <- fixef(lme_las)
las_fit_lme <- data_las %>% 
  mutate(
    lme.fixed = lme_las$fitted[, 1],
    lme.random = lme_las$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_las_lme <- ggpredict(lme_las, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_las <- ggplot() +
  geom_point(
    data = las_fit_lme,
    aes(x = year_new, y = las, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = las_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = las, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_las_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_path(
    data = las_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Las (days)") +
  labs(title = expression(paste(bold("G      "), italic("Extended Length of Allergy Season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

p_trend_space <- ggplot() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))
title1 <- ggdraw() + draw_label("Ecology Perspective", hjust = 0.5, vjust = 1, fontface = "bold")
p_trend_c1 <- plot_grid(title1, p_trend_peak, p_trend_integral, p_trend_sos, p_trend_los, ncol = 1,
                        rel_heights = c(0.1, 1, 1, 1, 1))
title2 <- ggdraw() + draw_label("Public Health Perspective", hjust = 0.5, vjust = 1, fontface = "bold")
p_trend_c2 <- plot_grid(title2, p_trend_space, p_trend_integral_as, p_trend_sas, p_trend_las, ncol = 1,
                        rel_heights = c(0.1, 1, 1, 1, 1))
p_trend <- plot_grid(p_trend_c1, p_trend_c2, ncol = 2)