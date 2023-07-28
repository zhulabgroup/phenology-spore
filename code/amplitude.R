df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_amplitude.rds"))
pct = 0.8
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
  ylab(expression("Amplitude (grains*m"^-3*")")) +
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




data_amplitude_climate <- data_amplitude %>% 
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
  drop_na(amplitude) %>% 
  filter(observ_pct >= pct) %>% 
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
lm_amplitude <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_amplitude_climate)
summary(lm_amplitude)





ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_amplitude_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = round(seq(min(data_amplitude_climate$Nyear), max(data_amplitude_climate$Nyear), length.out = 5)),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_amplitude_climate$rescaled_slope), min(data_amplitude_climate$rescaled_slope)/2, 0, max(data_amplitude_climate$rescaled_slope)/2, max(data_amplitude_climate$rescaled_slope)),
    labels = c(min(data_amplitude_climate$rescaled_slope), min(data_amplitude_climate$rescaled_slope)/2, 0, max(data_amplitude_climate$rescaled_slope)/2, max(data_amplitude_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nln(amplitude)"
  ) +
  geom_point(data = data_amplitude_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  guides(
    size = "none",
    shape = "none"
  ) +
  ggtitle("F") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 5, 10, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")



pct = 0.8
data_trough <- df_metrics %>% 
  filter(trough_check == 1) %>% 
  drop_na(trough) %>% 
  filter(observ_pct >= pct) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(trough) ~ year_new, .)
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
  filter(trough_check == 1) %>% 
  drop_na(trough) %>% 
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
lme_trough <- lme(
  log(trough) ~ year_new,
  data = data_trough,
  random = ~ year_new | n
)
summary(lme_trough)


data_trough_climate <- data_trough %>% 
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
  filter(trough_check == 1) %>% 
  drop_na(trough) %>% 
  filter(observ_pct >= pct) %>% 
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
lm_trough <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_trough_climate)
summary(lm_trough)

ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_trough_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = round(seq(min(data_trough_climate$Nyear), max(data_trough_climate$Nyear), length.out = 5)),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_trough_climate$rescaled_slope), min(data_trough_climate$rescaled_slope)/2, 0, max(data_trough_climate$rescaled_slope)/2, max(data_trough_climate$rescaled_slope)),
    labels = c(min(data_trough_climate$rescaled_slope), min(data_trough_climate$rescaled_slope)/2, 0, max(data_trough_climate$rescaled_slope)/2, max(data_trough_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nln(trough)"
  ) +
  geom_point(data = data_trough_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  guides(
    size = "none",
    shape = "none"
  ) +
  ggtitle("F") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 5, 10, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
