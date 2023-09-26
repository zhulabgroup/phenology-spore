# source("code/19_results_lme_trend.R")
# pct = 0.8



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
p_climate_1a <- ggplot() +
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
    range = c(5, 7),
    breaks = round(seq(min(data_peak_climate$Nyear), max(data_peak_climate$Nyear), length.out = 5)),
    name = "Number of year"
    ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
    labels = function(x) {
      ifelse(x == 0, "0", paste0(x, "\u00B3"))
    },
    name = "Temporal trend of\nln(peak concentration)"
    ) +
  geom_point(data = data_peak_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  guides(
    size = "none",
    shape = "none"
  ) +
  ggtitle("D") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 0, 0),
        plot.title = element_text(face = "bold"),
        legend.position = "left")

slope_lm_peak <- summary(lm_peak)$coefficients["slope_tap", "Estimate"]
p_lm_peak <- summary(lm_peak)$coefficients["slope_tap", "Pr(>|t|)"] %>% round(3)
data_peak_climate$predict <- summary(lm_peak)$coefficients["(Intercept)", "Estimate"] +
  summary(lm_peak)$coefficients["slope_mat", "Estimate"] * mean(data_peak_climate$slope_mat) +
  slope_lm_peak * data_peak_climate$slope_tap
peak_fix_var <- ggpredict(lm_peak, terms = c("slope_tap"), type = "re") %>%
  as_tibble()
p_climate_1b <- ggplot(data_peak_climate, aes(x = slope_tap)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_ribbon(
    data = peak_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    aes(y = predict),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    aes(y = slope, col = rescaled_slope)
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  xlab("Temporal trend of TAP") +
  ylab("Temporal trend of ln(peak)") +
  ggtitle("E") +
  theme(plot.title.position = "plot",
        plot.margin = margin(0, 20, 10, 0),
        plot.title = element_text(face = "bold")) +
  geom_text(
    aes(
      x = -21,
      y = 0.05,
      label = paste0(
        "P-value: ", p_lm_peak
      )
    ),
    hjust = 0.5,
    vjust = 0
  )


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
p_climate_2c <- ggplot() +
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
    range = c(5, 7),
    breaks = round(seq(min(data_integral_climate$Nyear), max(data_integral_climate$Nyear), length.out = 5)),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
    labels = function(x) {
      ifelse(x == 0, "0", paste0(x, "\u00B3"))
    },
    name = "Temporal trend of\nln(annual integral)"
  ) +
  geom_point(data = data_integral_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  guides(
    size = "none",
    shape = "none"
  ) +
  ggtitle("F") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 0, 0),
        plot.title = element_text(face = "bold"),
        legend.position = "left")

slope_lm_integral <- summary(lm_integral)$coefficients["slope_tap", "Estimate"]
p_lm_integral <- summary(lm_integral)$coefficients["slope_tap", "Pr(>|t|)"] %>% round(3)
data_integral_climate$predict <- summary(lm_integral)$coefficients["(Intercept)", "Estimate"] +
  summary(lm_integral)$coefficients["slope_mat", "Estimate"] * mean(data_integral_climate$slope_mat) +
  slope_lm_integral * data_integral_climate$slope_tap
integral_fix_var <- ggpredict(lm_integral, terms = c("slope_tap"), type = "re") %>%
  as_tibble()
p_climate_2d <- ggplot(data_integral_climate, aes(x = slope_tap)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_ribbon(
    data = integral_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    aes(y = predict),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    aes(y = slope, col = rescaled_slope)
  ) +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  xlab("Temporal trend of TAP") +
  ylab("Temporal trend of ln(AIn)") +
  ggtitle("G") +
  theme(plot.title.position = "plot",
        plot.margin = margin(0, 20, 10, 0),
        plot.title = element_text(face = "bold")) +
  geom_text(
    aes(
      x = -28,
      y = 0.05,
      label = paste0(
        "P-value: ", p_lm_integral
      )
    ),
    hjust = 0.5,
    vjust = 0
  )






# ## allergy season integral
# # filter data
# data_integral_as_climate <- data_integral_as %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>%
#   do({
#     result <- lm(tap ~ year_new, .)
#     data_frame(
#       r_squared_tap =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_tap =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
#   ungroup() %>%
#   right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>%
#   filter(state != "PR") %>% 
#   filter(country == "US") %>% 
#   drop_na(integral_as) %>% 
#   filter(observ_pct_as >= pct) %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
#   mutate(start_year = min(year_new)) %>% 
#   mutate(end_year = max(year_new)) %>% 
#   mutate(Nyear = end_year - start_year + 1) %>%
#   mutate(Nrcd = n()) %>% 
#   filter(Nrcd >= 5) %>% 
#   ungroup() %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
#   do({
#     result <- lm(mat ~ year_new, .)
#     data_frame(
#       r_squared_mat =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_mat =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
#   ungroup() %>% 
#   mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
# # fit lm
# lm_integral_as <- lm(
#   slope ~ 1 + slope_mat + slope_tap,
#   data = data_integral_as_climate)
# # plot
# p_climate_c1 <- ggplot() +
#   geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
#   geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
#   coord_map("conic", lat0 = 30) +
#   theme_void() +
#   geom_point(
#     data = data_integral_as_climate, 
#     aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_integral_as_climate$Nyear), max(data_integral_as_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope)),
#     labels = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of\nln(as integral)"
#   ) +
#   geom_point(data = data_integral_as_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
#   ggtitle("E") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold"),
#         legend.position = "left")
# p_climate_c2 <- ggplot() +
#   geom_point(
#     data = data_integral_as_climate,
#     aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_integral_as_climate$Nyear), max(data_integral_as_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope)),
#     labels = c(min(data_integral_as_climate$rescaled_slope), min(data_integral_as_climate$rescaled_slope)/2, 0, max(data_integral_as_climate$rescaled_slope)/2, max(data_integral_as_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of ln(allergy season integral)"
#   ) +
#   geom_point(data = data_integral_as_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   ggtitle("F") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold")) +
#   xlab("Temporal trend of tap") +
#   ylab("Temporal trend of mat")
# 
# 
# 
# ## sos
# # filter data
# data_sos_climate <- data_sos %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>%
#   do({
#     result <- lm(tap ~ year_new, .)
#     data_frame(
#       r_squared_tap =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_tap =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
#   ungroup() %>%
#   right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>%
#   filter(state != "PR") %>% 
#   filter(country == "US") %>% 
#   drop_na(sos) %>% 
#   filter(observ_pct >= 1) %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
#   mutate(start_year = min(year_new)) %>% 
#   mutate(end_year = max(year_new)) %>% 
#   mutate(Nyear = end_year - start_year + 1) %>%
#   mutate(Nrcd = n()) %>% 
#   filter(Nrcd >= 5) %>% 
#   ungroup() %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
#   do({
#     result <- lm(mat ~ year_new, .)
#     data_frame(
#       r_squared_mat =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_mat =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
#   ungroup() %>% 
#   mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
# # fit lm
# lm_sos <- lm(
#   slope ~ 1 + slope_mat + slope_tap,
#   data = data_sos_climate)
# # plot
# p_climate_d1 <- ggplot() +
#   geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
#   geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
#   coord_map("conic", lat0 = 30) +
#   theme_void() +
#   geom_point(
#     data = data_sos_climate, 
#     aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_sos_climate$Nyear), max(data_sos_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "red", mid= "white", high = "blue", midpoint = 0,
#     breaks = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope)),
#     labels = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of\nsos"
#   ) +
#   geom_point(data = data_sos_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
#   ggtitle("G") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold"),
#         legend.position = "left")
# p_climate_d2 <- ggplot() +
#   geom_point(
#     data = data_sos_climate,
#     aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_sos_climate$Nyear), max(data_sos_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "red", mid= "white", high = "blue", midpoint = 0,
#     breaks = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope)),
#     labels = c(min(data_sos_climate$rescaled_slope), min(data_sos_climate$rescaled_slope)/2, 0, max(data_sos_climate$rescaled_slope)/2, max(data_sos_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of sos"
#   ) +
#   geom_point(data = data_sos_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   ggtitle("H") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold")) +
#   xlab("Temporal trend of tap") +
#   ylab("Temporal trend of mat")
# 
# 
# 
# ## los
# # filter data
# data_los_climate <- data_los %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>%
#   do({
#     result <- lm(tap ~ year_new, .)
#     data_frame(
#       r_squared_tap =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_tap =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
#   ungroup() %>%
#   right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>%
#   filter(state != "PR") %>% 
#   filter(country == "US") %>% 
#   drop_na(los) %>% 
#   filter(observ_pct >= 1) %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
#   mutate(start_year = min(year_new)) %>% 
#   mutate(end_year = max(year_new)) %>% 
#   mutate(Nyear = end_year - start_year + 1) %>%
#   mutate(Nrcd = n()) %>% 
#   filter(Nrcd >= 5) %>% 
#   ungroup() %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
#   do({
#     result <- lm(mat ~ year_new, .)
#     data_frame(
#       r_squared_mat =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_mat =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
#   ungroup() %>% 
#   mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
# # fit lm
# lm_los <- lm(
#   slope ~ 1 + slope_mat + slope_tap,
#   data = data_los_climate)
# # plot
# p_climate_e1 <- ggplot() +
#   geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
#   geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
#   coord_map("conic", lat0 = 30) +
#   theme_void() +
#   geom_point(
#     data = data_los_climate, 
#     aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_los_climate$Nyear), max(data_los_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope)),
#     labels = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of\nlos"
#   ) +
#   geom_point(data = data_los_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
#   ggtitle("I") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold"),
#         legend.position = "left")
# p_climate_e2 <- ggplot() +
#   geom_point(
#     data = data_los_climate,
#     aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_los_climate$Nyear), max(data_los_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope)),
#     labels = c(min(data_los_climate$rescaled_slope), min(data_los_climate$rescaled_slope)/2, 0, max(data_los_climate$rescaled_slope)/2, max(data_los_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of los"
#   ) +
#   geom_point(data = data_los_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   ggtitle("J") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold")) +
#   xlab("Temporal trend of tap") +
#   ylab("Temporal trend of mat")
# 


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
p_climate_3e <- ggplot() +
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
    range = c(5, 7),
    breaks = round(seq(min(data_sas_climate$Nyear), max(data_sas_climate$Nyear), length.out = 5)),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(-15, -5, -1, 0, 1, 6) %>% {
      sign(.) * abs(.)^(1/3)
    },
    labels = c(-15, -5, -1, 0, 1, 6),
    name = "Temporal trend of\nstart of allergy season"
  ) +
  geom_point(data = data_sas_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  guides(
    size = "none",
    shape = "none"
  ) +
  ggtitle("A") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10),
        plot.title = element_text(face = "bold"),
        legend.position = "left")

slope_lm_sas_mat <- summary(lm_sas)$coefficients["slope_mat", "Estimate"]
p_lm_sas_mat <- summary(lm_sas)$coefficients["slope_mat", "Pr(>|t|)"] %>% round(3)
data_sas_climate$predict <- summary(lm_sas)$coefficients["(Intercept)", "Estimate"] +
  summary(lm_sas)$coefficients["slope_tap", "Estimate"] * mean(data_sas_climate$slope_tap) +
  slope_lm_sas_mat * data_sas_climate$slope_mat
sas_fix_var <- ggpredict(lm_sas, terms = c("slope_mat"), type = "re") %>%
  as_tibble()
p_climate_3f <- ggplot(data_sas_climate, aes(x = slope_mat)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_ribbon(
    data = sas_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    aes(y = predict),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    aes(y = slope, col = rescaled_slope)
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  xlab("Temporal trend of MAT") +
  ylab("Temporal trend of SAS") +
  ggtitle("B") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 10, 0),
        plot.title = element_text(face = "bold")) +
  geom_text(
    aes(
      x = 0.2,
      y = 10,
      label = paste0(
        "P-value: ", p_lm_sas_mat
      )
    ),
    hjust = 0.5,
    vjust = 0
  )

slope_lm_sas_tap <- summary(lm_sas)$coefficients["slope_tap", "Estimate"]
p_lm_sas_tap <- summary(lm_sas)$coefficients["slope_tap", "Pr(>|t|)"] %>% round(3)
data_sas_climate$predict <- summary(lm_sas)$coefficients["(Intercept)", "Estimate"] +
  summary(lm_sas)$coefficients["slope_mat", "Estimate"] * mean(data_sas_climate$slope_mat) +
  slope_lm_sas_tap * data_sas_climate$slope_tap
sas_fix_var <- ggpredict(lm_sas, terms = c("slope_tap"), type = "re") %>%
  as_tibble()
p_climate_3g <- ggplot(data_sas_climate, aes(x = slope_tap)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_ribbon(
    data = sas_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    aes(y = predict),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    aes(y = slope, col = rescaled_slope)
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  xlab("Temporal trend of TAP") +
  ylab("Temporal trend of SAS") +
  ggtitle("C") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 10, 10, 0),
        plot.title = element_text(face = "bold")) +
  geom_text(
    aes(
      x = 6,
      y = 10,
      label = paste0(
        "P-value: ", p_lm_sas_tap
      )
    ),
    hjust = 0.5,
    vjust = 0.5
  )



p_climate_space <- ggplot() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))
p_climate_4 <- ggplot() +
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
    range = c(5, 7),
    breaks = round(seq(min(data_sas_climate$Nyear), max(data_sas_climate$Nyear), length.out = 5)),
    name = "Number of year"
  ) +
  scale_color_gradient2(
    low = "red", mid= "white", high = "blue", midpoint = 0,
    breaks = c(-15, -5, -1, 0, 1, 6) %>% {
      sign(.) * abs(.)^(1/3)
    },
    labels = c(-15, -5, -1, 0, 1, 6),
    name = "Temporal trend of\nsas",
  ) +
  guides(color = "none") +
  geom_point(data = data_sas_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("A") +
  theme(plot.title.position = "plot",
        plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical")
legend_grob <- get_legend(p_climate_4)
p_climate_lgd <- plot_grid(
  legend_grob, p_climate_space,
  ncol = 2,
  rel_widths = c(
    3, 1
  ))

legend_1 <- get_legend(p_climate_1a)
p_climate_1_2 <- plot_grid(
  legend_1, p_climate_1b,
  ncol = 2,
  rel_widths = c(
    1, 2
  ))
p_climate_1 <- plot_grid(
  p_climate_1a + theme(legend.position = "none"), p_climate_1_2,
  nrow = 2,
  rel_heights = c(1, 1)
  )

legend_2 <- get_legend(p_climate_2c)
p_climate_2_2 <- plot_grid(
  legend_2, p_climate_2d,
  ncol = 2,
  rel_widths = c(
    1, 2
  ))
p_climate_2 <- plot_grid(
  p_climate_2c + theme(legend.position = "none"), p_climate_2_2,
  nrow = 2,
  rel_heights = c(1, 1)
)

p_climate_3r <- plot_grid(
  p_climate_3f, p_climate_3g,
  ncol = 1,
  rel_heights = c(
    1, 1
  ))
p_climate_3 <- plot_grid(
  p_climate_3e, p_climate_3r,
  ncol = 2,
  rel_widths = c(
    3, 1
  ))

p_climate_4 <- plot_grid(
  p_climate_1, p_climate_2,
  ncol = 2,
  rel_widths = c(
    1, 1
  ))

p_climate <- plot_grid(
  p_climate_3, p_climate_4, legend_grob,
  ncol = 1,
  rel_heights = c(
    4.5, 6, 3
  ))
# ## las
# # filter data
# data_las_climate <- data_las %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>%
#   do({
#     result <- lm(tap ~ year_new, .)
#     data_frame(
#       r_squared_tap =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_tap =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
#   ungroup() %>%
#   right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>%
#   filter(state != "PR") %>% 
#   filter(country == "US") %>% 
#   drop_na(las) %>% 
#   filter(observ_pct_as >= pct) %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
#   mutate(start_year = min(year_new)) %>% 
#   mutate(end_year = max(year_new)) %>% 
#   mutate(Nyear = end_year - start_year + 1) %>%
#   mutate(Nrcd = n()) %>% 
#   filter(Nrcd >= 5) %>% 
#   ungroup() %>% 
#   group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd, r_squared, p_value, intercept, slope, r_squared_tap, p_value_tap, intercept_tap, slope_tap) %>%
#   do({
#     result <- lm(mat ~ year_new, .)
#     data_frame(
#       r_squared_mat =
#         result %>%
#         summary() %>%
#         magrittr::use_series(adj.r.squared),
#       p_value_mat =
#         result %>%
#         anova() %>%
#         magrittr::use_series(`Pr(>F)`) %>%
#         magrittr::extract2(1)
#     ) %>%
#       bind_cols(
#         result %>%
#           coef() %>%
#           as.list() %>%
#           as_data_frame()
#       )
#   }) %>%
#   rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
#   ungroup() %>% 
#   mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
# # fit lm
# lm_las <- lm(
#   slope ~ 1 + slope_mat + slope_tap,
#   data = data_las_climate)
# # plot
# p_climate_g1 <- ggplot() +
#   geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
#   geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
#   coord_map("conic", lat0 = 30) +
#   theme_void() +
#   geom_point(
#     data = data_las_climate, 
#     aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_las_climate$Nyear), max(data_las_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope)),
#     labels = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of\nlas"
#   ) +
#   geom_point(data = data_las_climate, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
#   ggtitle("M") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold"),
#         legend.position = "left")
# p_climate_g2 <- ggplot() +
#   geom_point(
#     data = data_las_climate,
#     aes(x = slope_tap, y = slope_mat, size = Nyear, color = rescaled_slope),
#     alpha = 0.8
#   ) +
#   scale_size_continuous(
#     range = c(3, 5),
#     breaks = seq(min(data_las_climate$Nyear), max(data_las_climate$Nyear), by = 3),
#     name = "Number of year"
#   ) +
#   scale_color_gradient2(
#     low = "blue", mid= "white", high = "red", midpoint = 0,
#     breaks = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope)),
#     labels = c(min(data_las_climate$rescaled_slope), min(data_las_climate$rescaled_slope)/2, 0, max(data_las_climate$rescaled_slope)/2, max(data_las_climate$rescaled_slope))^3 %>% round(4),
#     name = "Temporal trend of las"
#   ) +
#   geom_point(data = data_las_climate, aes(x = slope_tap, y = slope_mat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
#   scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value")) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   ggtitle("N") +
#   theme(plot.title.position = "plot",
#         plot.margin = margin(10, 10, 20, 10),
#         plot.title = element_text(face = "bold")) +
#   xlab("Temporal trend of tap") +
#   ylab("Temporal trend of mat")