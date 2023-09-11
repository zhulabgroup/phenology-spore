# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_daymet.rds"))
# pct = 0.8

df_metrics <- df_metrics %>% 
  mutate(peak = ifelse(peak_check == 0, NA, peak)) %>% 
  mutate(peak_doy = ifelse(peak_check == 0, NA, peak_doy)) %>% 
  mutate(peak_date_old = ifelse(peak_check == 0, NA, peak_date_old))

## peak
# filter data
data_peak <- df_metrics %>% 
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
slope_peak <- fixef(lme_peak)[["year_new"]] %>% round(3)
p_peak <- summary(lme_peak)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = peak_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab(expression("Peak concentration (grains m"^-3*")")) +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("H      \n"), italic("Decreased\npeak concentration")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 11,
      label = paste0(
        "\nSlope: ", slope_peak,
        "\nP-value: ", p_peak
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
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
slope_integral <- fixef(lme_integral)[["year_new"]] %>% round(3)
p_integral <- summary(lme_integral)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = integral_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab(expression("Annual integral (grains m"^-3*" days)")) +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("I      \n"), italic("Decreased\nannual integral")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 16.5,
      label = paste0(
        "\nSlope: ", slope_integral,
        "\nP-value: ", p_integral
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
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
slope_integral_as <- fixef(lme_integral_as)[["year_new"]] %>% round(3)
p_integral_as <- summary(lme_integral_as)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = integral_as_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab(expression("Allergy season integral (grains m"^-3*" days)")) +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("J      \n"), italic("Increased allergy\nseason integral")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 16.5,
      label = paste0(
        "\nSlope: ", slope_integral_as,
        "\nP-value: ", p_integral_as
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
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
slope_sos <- fixef(lme_sos)[["year_new"]] %>% round(3)
p_sos <- summary(lme_sos)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = sos_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Start of season (day of spore year)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("A      \n"), italic("Advanced\nspore season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 120,
      label = paste0(
        "\nSlope: ", slope_sos,
        "\nP-value: ", p_sos
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
  )



## eos
# filter data
data_eos <- df_metrics %>% 
  drop_na(eos) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(eos ~ year_new, .)
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
  drop_na(eos) %>% 
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
lme_eos <- lme(
  eos ~ year_new,
  data = data_eos,
  random = ~ year_new | n
)
slope_eos <- fixef(lme_eos)[["year_new"]] %>% round(3)
p_eos <- summary(lme_eos)$tTable[["year_new", "p-value"]] %>% round(3)
eos_fit_lme <- data_eos %>% 
  mutate(
    lme.fixed = lme_eos$fitted[, 1],
    lme.random = lme_eos$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_eos_lme <- ggpredict(lme_eos, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_eos <- ggplot() +
  geom_point(
    data = eos_fit_lme,
    aes(x = year_new, y = eos, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = eos_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = eos, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_eos_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = eos_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("End of season (day of spore year)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("C      \n"), italic("Advanced end\nof spore season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 350,
      label = paste0(
        "\nSlope: ", slope_eos,
        "\nP-value: ", p_eos
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
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
slope_los <- fixef(lme_los)[["year_new"]] %>% round(3)
p_los <- summary(lme_los)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = los_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Length of season (days)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("E      \n"), italic("Extended\nspore season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 290,
      label = paste0(
        "\nSlope: ", slope_los,
        "\nP-value: ", p_los
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
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
slope_sas <- fixef(lme_sas)[["year_new"]] %>% round(3)
p_sas <- summary(lme_sas)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = sas_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Start of allergy season (day of spore year)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("B      \n "), italic("Advanced\nallergy season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 320,
      label = paste0(
        "\nSlope: ", slope_sas,
        "\nP-value: ", p_sas
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
  )




## eas
# filter data
data_eas <- df_metrics %>% 
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
  drop_na(eas) %>% 
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
lme_eas <- lme(
  eas ~ year_new,
  data = data_eas,
  random = ~ year_new | n
)
slope_eas <- fixef(lme_eas)[["year_new"]] %>% round(3)
p_eas <- summary(lme_eas)$tTable[["year_new", "p-value"]] %>% round(3)
eas_fit_lme <- data_eas %>% 
  mutate(
    lme.fixed = lme_eas$fitted[, 1],
    lme.random = lme_eas$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_eas_lme <- ggpredict(lme_eas, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
# plot
p_trend_eas <- ggplot() +
  geom_point(
    data = eas_fit_lme,
    aes(x = year_new, y = eas, col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = eas_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = eas, group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_eas_lme,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = eas_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("End of allergy season (day of spore year)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("D      \n "), italic("Advanced end\nof allergy season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 370,
      label = paste0(
        "\nSlope: ", slope_eas,
        "\nP-value: ", p_eas
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
  )



## las
# filter data
data_las <- df_metrics %>% 
  drop_na(las) %>% 
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
slope_las <- fixef(lme_las)[["year_new"]] %>% round(3)
p_las <- summary(lme_las)$tTable[["year_new", "p-value"]] %>% round(3)
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
    fill = "black",
    alpha = 0.2
  ) +
  geom_line(
    data = las_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1, linetype = "dashed"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Length of allergy season (days)") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("F      \n"), italic("Extended\nallergy season")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 365,
      label = paste0(
        "\nSlope: ", slope_las,
        "\nP-value: ", p_las
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
  )




data_amplitude <- df_metrics %>% 
  drop_na(amplitude) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(amplitude) ~ year_new, .)
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
  drop_na(amplitude) %>% 
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
lme_amplitude <- lme(
  log(amplitude) ~ year_new,
  data = data_amplitude,
  random = ~ year_new | n
)
# summary(lme_amplitude)
slope_amplitude <- fixef(lme_amplitude)[["year_new"]] %>% round(3)
p_amplitude <- summary(lme_amplitude)$tTable[["year_new", "p-value"]] %>% round(3)
amplitude_fit_lme <- data_amplitude %>% 
  mutate(
    lme.fixed = lme_amplitude$fitted[, 1],
    lme.random = lme_amplitude$fitted[, 2]
  ) %>%
  mutate(n = as.character(n)) %>% 
  as_tibble()
ci_amplitude_lme <- ggpredict(lme_amplitude, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()
p_trend_amplitude <- ggplot() +
  geom_point(
    data = amplitude_fit_lme,
    aes(x = year_new, y = log(amplitude), col = n),
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    data = amplitude_fit_lme,
    method = "lm",
    se = FALSE,
    aes(x = year_new, y = log(amplitude), group = n, col = n),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = ci_amplitude_lme,
    aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
    fill = "black",
    alpha = 0.2
  ) +
  geom_path(
    data = amplitude_fit_lme,
    aes(x = year_new, y = lme.fixed),
    col = "black", linewidth = 1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab(expression("Amplitude (grains m"^-3*")")) +
  scale_y_continuous(labels = scales::math_format(e^.x)) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("G      \n"), italic("Decreased\namplitude")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 10)
  ) +
  geom_text(
    aes(
      x = 2003,
      y = 11,
      label = paste0(
        "\nSlope: ", slope_amplitude,
        "\nP-value: ", p_amplitude
      )
    ),
    hjust = 0,
    vjust = 0.5,
    col = "black"
  )








p_trend_space <- ggplot() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))
title1 <- ggdraw() + draw_label("Ecology Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
title2 <- ggdraw() + draw_label("Public Health Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
p_trend_r1 <- plot_grid(p_trend_sos, p_trend_eos, p_trend_los, p_trend_amplitude, p_trend_integral,
                        nrow = 1,
                        rel_widths = c(1, 1, 1, 1, 1))
p_trend_r2 <- plot_grid(p_trend_sas, p_trend_eas, p_trend_las, p_trend_peak, p_trend_integral_as,
                        nrow = 1,
                        rel_widths = c(1, 1, 1, 1, 1))
p_trend <- plot_grid(title1,
                     p_trend_r1,
                     title2,
                     p_trend_r2,
                     nrow = 4,
                     rel_heights = c(0.1, 1, 0.1, 1))
