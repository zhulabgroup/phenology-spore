# source("code/19_results_lme_trend.R")
# pct = 1



## peak
# filter data
data_peak_climate <- data_peak %>% 
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
  drop_na(peak) %>% 
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
lm_peak <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_peak_climate)
# plot
p_climate_a1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_peak_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
    ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_peak_climate$Nyear), max(data_peak_climate$Nyear), by = 3),
    name = "Number of year"
    ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_peak_climate$rescaled_slope), min(data_peak_climate$rescaled_slope)/2, 0, max(data_peak_climate$rescaled_slope)/2, max(data_peak_climate$rescaled_slope)),
    labels = c(min(data_peak_climate$rescaled_slope), min(data_peak_climate$rescaled_slope)/2, 0, max(data_peak_climate$rescaled_slope)/2, max(data_peak_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nln(peak)"
    ) +
  geom_point(data = data_peak_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("A") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_a2 <- ggplot() +
  geom_point(
    data = data_peak_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_peak_climate$Nyear), max(data_peak_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_peak_climate$rescaled_slope), min(data_peak_climate$rescaled_slope)/2, 0, max(data_peak_climate$rescaled_slope)/2, max(data_peak_climate$rescaled_slope)),
    labels = c(min(data_peak_climate$rescaled_slope), min(data_peak_climate$rescaled_slope)/2, 0, max(data_peak_climate$rescaled_slope)/2, max(data_peak_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of ln(peak)"
  ) +
  geom_point(data = data_peak_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("B") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## annual integral
# filter data
data_integral_climate <- data_integral %>% 
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
lm_integral <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_integral_climate)
# plot
p_climate_b1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_integral_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_integral_climate$Nyear), max(data_integral_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_integral_climate$rescaled_slope), min(data_integral_climate$rescaled_slope)/2, 0, max(data_integral_climate$rescaled_slope)/2, max(data_integral_climate$rescaled_slope)),
    labels = c(min(data_integral_climate$rescaled_slope), min(data_integral_climate$rescaled_slope)/2, 0, max(data_integral_climate$rescaled_slope)/2, max(data_integral_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nln(annual integral)"
  ) +
  geom_point(data = data_integral_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("C") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_b2 <- ggplot() +
  geom_point(
    data = data_integral_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_integral_climate$Nyear), max(data_integral_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_integral_climate$rescaled_slope), min(data_integral_climate$rescaled_slope)/2, 0, max(data_integral_climate$rescaled_slope)/2, max(data_integral_climate$rescaled_slope)),
    labels = c(min(data_integral_climate$rescaled_slope), min(data_integral_climate$rescaled_slope)/2, 0, max(data_integral_climate$rescaled_slope)/2, max(data_integral_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of ln(annual integral)"
  ) +
  geom_point(data = data_integral_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("D") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## allergy season integral
# filter data
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
# plot
p_climate_c1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_integral_as_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_integral_as_climate$Nyear), max(data_integral_as_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope)),
    labels = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nln(as integral)"
  ) +
  geom_point(data = data_integral_as_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("E") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_c2 <- ggplot() +
  geom_point(
    data = data_integral_as_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_integral_as_climate$Nyear), max(data_integral_as_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope)),
    labels = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of ln(allergy season integral)"
  ) +
  geom_point(data = data_integral_as_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("F") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## sos
# filter data
data_sos_climate <- data_sos %>% 
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
  filter(observ_pct >= 1) %>% 
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
lm_sos <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_sos_climate)
# plot
p_climate_d1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_sos_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_sos_climate$Nyear), max(data_sos_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope)),
    labels = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nsos"
  ) +
  geom_point(data = data_sos_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("G") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_d2 <- ggplot() +
  geom_point(
    data = data_sos_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_sos_climate$Nyear), max(data_sos_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope)),
    labels = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of sos"
  ) +
  geom_point(data = data_sos_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("H") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## los
# filter data
data_los_climate <- data_los %>% 
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
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>% 
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
lm_los <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_los_climate)
# plot
p_climate_e1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_los_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_los_climate$Nyear), max(data_los_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope)),
    labels = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nlos"
  ) +
  geom_point(data = data_los_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("I") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_e2 <- ggplot() +
  geom_point(
    data = data_los_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_los_climate$Nyear), max(data_los_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope)),
    labels = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of los"
  ) +
  geom_point(data = data_los_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("J") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## sas
# filter data
data_sas_climate <- data_sas %>% 
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
  drop_na(sas) %>% 
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
lm_sas <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_sas_climate)
# plot
p_climate_f1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_sas_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_sas_climate$Nyear), max(data_sas_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(min(data_sas_climate$rescaled_slope), min(data_sas_climate$rescaled_slope)/2, 0, max(data_sas_climate$rescaled_slope)/2, max(data_sas_climate$rescaled_slope)),
    labels = c(min(data_sas_climate$rescaled_slope), min(data_sas_climate$rescaled_slope)/2, 0, max(data_sas_climate$rescaled_slope)/2, max(data_sas_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nsas"
  ) +
  geom_point(data = data_sas_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("K") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_f2 <- ggplot() +
  geom_point(
    data = data_sas_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_sas_climate$Nyear), max(data_sas_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(min(data_sas_climate$rescaled_slope), min(data_sas_climate$rescaled_slope)/2, 0, max(data_sas_climate$rescaled_slope)/2, max(data_sas_climate$rescaled_slope)),
    labels = c(min(data_sas_climate$rescaled_slope), min(data_sas_climate$rescaled_slope)/2, 0, max(data_sas_climate$rescaled_slope)/2, max(data_sas_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nsas"
  ) +
  geom_point(data = data_sas_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("L") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")



## las
# filter data
data_las_climate <- data_las %>% 
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
lm_las <- lm(
  slope ~ 1 + slope_mat + slope_tap,
  data = data_las_climate)
# plot
p_climate_g1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_las_climate, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_las_climate$Nyear), max(data_las_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope)),
    labels = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of\nlas"
  ) +
  geom_point(data = data_las_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("M") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
p_climate_g2 <- ggplot() +
  geom_point(
    data = data_las_climate,
    aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(3, 5),
    breaks = seq(min(data_las_climate$Nyear), max(data_las_climate$Nyear), by = 3),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope)),
    labels = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope))^3 %>% round(4),
    name = "Temporal trend of las"
  ) +
  geom_point(data = data_las_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("N") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 20, 10),
        plot.title = element_text(face = "bold")) +
  xlab("Temporal trend of tap") +
  ylab("Temporal trend of mat")