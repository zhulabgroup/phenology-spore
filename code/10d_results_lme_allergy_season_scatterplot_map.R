df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

## peak
# filter data
data_peak <- df_metrics %>% 
  filter(peak_check == 1) %>%
  filter(observ_pct >= 0.7) %>% 
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
  filter(peak_check == 1) %>%
  filter(observ_pct >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()
view(data_peak %>% distinct(n, .keep_all = T))

# fit lme
lme_peak <- lme(
  # peak ~ year_new,
  log(peak) ~ year_new,
  data = data_peak
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
  )
summary(lme_peak)
peak_fit <- data_peak %>% 
  # filter(state != "PR") %>%
  mutate(
    peak_fit.fixed = lme_peak$fitted[, 1],
    peak_fit.random = lme_peak$fitted[, 2]
  ) %>%
  as_tibble()

# # check residuals
# df_residuals <- residuals(lme_peak) %>% 
#   data.frame() %>% 
#   rename("residual" = ".")
# p23 <- ggplot(df_residuals, aes(x = residual)) +
#   geom_density(color = "black") +
#   labs(x = "Residuals", y = "Density") +
#   ggtitle(paste0("data completeness: 70%", "\nlog(peak) ~ year")) +
#   theme_classic()
# p24 <- ggplot(df_residuals, aes(sample = residual)) +
#   geom_qq() +
#   geom_qq_line(color = "red") +
#   labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
#   ggtitle(paste0("slope = -0.004749 ", "\np-value = 0.3549")) +
#   # ggtitle("QQ Plot of Residuals") +
#   theme_classic()
# pdf(
#   "output/figures/p_residuals_peak.pdf",
#   width = 8,
#   height = 8*0.618
# )
# grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
# grid.arrange(p7, p9, p11, p8, p10, p12, ncol = 3)
# grid.arrange(p13, p15, p17, p14, p16, p18, ncol = 3)
# grid.arrange(p19, p21, p23, p20, p22, p24, ncol = 3)
# dev.off()

# map & scatterplot
p7 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_peak %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_peak %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_peak %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p8 <- ggplot(
  data = data_peak %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = log(peak))
  ) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = peak_fit,
    aes(x = year_new, y = peak_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\n70% data completeness",
    "\ntemporal trends of log(peak)",
    "\n17/23 decreasing",
    "\nslope = -0.004749",
    "\np-value = 0.3549"))
pdf(
  "output/figures/p_map_scatter_log(peak).pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
grid.arrange(p7, p8, ncol = 2, widths = c(2, 1))
dev.off()



## peak date
# filter data
data_peak_doy <- df_metrics %>% 
  filter(peak_check == 1) %>%
  # filter(observ_pct >= 0.7) %>%
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
  drop_na(peak_doy) %>% 
  filter(peak_check == 1) %>%
  # filter(observ_pct >= 0.7) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()
view(data_peak_doy %>% distinct(n, .keep_all = T))

# fit lme
lme_peak_doy <- lme(
  peak_doy ~ year_new,
  data = data_peak_doy
  %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_peak_doy)
peak_doy_fit <- data_peak_doy %>% 
  # filter(state != "PR") %>%
  mutate(
    peak_doy_fit.fixed = lme_peak_doy$fitted[, 1],
    peak_doy_fit.random = lme_peak_doy$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p9 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_peak_doy %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "red", mid= "white", high = "blue", midpoint = 0) +
  geom_point(data = data_peak_doy %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_peak_doy %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p10 <- ggplot(
  data = data_peak_doy %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = peak_doy)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = peak_doy_fit,
    aes(x = year_new, y = peak_doy_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ylab("peak date (Julian day)") +
  ggtitle(paste0(
    "\nno requirement on data completeness",
    "\ntemporal trends of peak date",
    "\n24/42 advanced",
    "\nslope = -0.6348",
    "\np-value = 0.0850"))
pdf(
  "output/figures/p_map_scatter_peak_date.pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
grid.arrange(p7, p8, ncol = 2, widths = c(2, 1))
grid.arrange(p9, p10, ncol = 2, widths = c(2, 1))
dev.off()



## annual integral
# filter data
data_integral <- df_metrics %>% 
  filter(observ_pct >= 0.7) %>% 
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
  filter(observ_pct >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()
view(data_integral %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_integral <- lme(
  # integral ~ year_new,
  log(integral) ~ year_new,
  data = data_integral
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_integral)
integral_fit <- data_integral %>% 
  # filter(state != "PR") %>%
  mutate(
    integral_fit.fixed = lme_integral$fitted[, 1],
    integral_fit.random = lme_integral$fitted[, 2]
  ) %>%
  as_tibble()

# # check residuals
# df_residuals <- residuals(lme_integral) %>% 
#   data.frame() %>% 
#   rename("residual" = ".")
# p23 <- ggplot(df_residuals, aes(x = residual)) +
#   geom_density(color = "black") +
#   labs(x = "Residuals", y = "Density") +
#   ggtitle(paste0("data completeness: 70%", "\nlog(integral) ~ year")) +
#   theme_classic()
# p24 <- ggplot(df_residuals, aes(sample = residual)) +
#   geom_qq() +
#   geom_qq_line(color = "red") +
#   labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
#   ggtitle(paste0("slope = -0.00930", "\np-value = 0.0594")) +
#   # ggtitle("QQ Plot of Residuals") +
#   theme_classic()
# pdf(
#   "output/figures/p_residuals_integral.pdf",
#   width = 10,
#   height = 10*0.618
# )
# grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
# grid.arrange(p7, p9, p11, p8, p10, p12, ncol = 3)
# grid.arrange(p13, p15, p17, p14, p16, p18, ncol = 3)
# grid.arrange(p19, p21, p23, p20, p22, p24, ncol = 3)
# dev.off()

# map & scatterplot
p7 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_integral %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_integral %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_integral %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p8 <- ggplot(
  data = data_integral %>% 
    mutate(n = as.character(n)),
  aes(x = year_new, y = log(integral))
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = integral_fit,
    aes(x = year_new, y = integral_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ylab("log(integral)") +
  ggtitle(paste0(
    "\n70% data completeness",
    "\ntemporal trends of log(integral)",
    "\n18/24 decreased",
    "\nslope = -0.00930",
    "\np-value = 0.0594"))
pdf(
  "output/figures/p_map_scatter_log(integral).pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
grid.arrange(p7, p8, ncol = 2, widths = c(2, 1))
dev.off()



## sos
# filter data
data_sos <- df_metrics %>% 
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
  ungroup()
view(data_sos %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_sos <- lme(
  sos ~ year_new,
  data = data_sos
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sos)
sos_fit <- data_sos %>% 
  # filter(state != "PR") %>%
  mutate(
    sos_fit.fixed = lme_sos$fitted[, 1],
    sos_fit.random = lme_sos$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_sos %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "red", mid= "white", high = "blue", midpoint = 0) +
  geom_point(data = data_sos %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_sos %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p2 <- ggplot(
  data = data_sos %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = sos)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = sos_fit,
    aes(x = year_new, y = sos_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\n100% data completeness",
    "\ntemporal trends of sos",
    "\n11/17 advanced",
    "\nslope = -0.3872",
    "\np-value = 0.0549"))



## eos
# filter data
data_eos <- df_metrics %>% 
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
  ungroup()
view(data_eos %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_eos <- lme(
  eos ~ year_new,
  data = data_eos
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_eos)
eos_fit <- data_eos %>% 
  # filter(state != "PR") %>%
  mutate(
    eos_fit.fixed = lme_eos$fitted[, 1],
    eos_fit.random = lme_eos$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p3 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_eos %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "red", mid= "white", high = "blue", midpoint = 0) +
  geom_point(data = data_eos %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_eos %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p4 <- ggplot(
  data = data_eos %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = eos)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = eos_fit,
    aes(x = year_new, y = eos_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\n100% data completeness",
    "\ntemporal trends of eos",
    "\n10/17 advanced",
    "\nslope = -0.0781",
    "\np-value = 0.6587"))



## los
# filter data
data_los <- df_metrics %>% 
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
  ungroup()
view(data_los %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_los <- lme(
  los ~ year_new,
  data = data_los
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_los)
los_fit <- data_los %>% 
  # filter(state != "PR") %>%
  mutate(
    los_fit.fixed = lme_los$fitted[, 1],
    los_fit.random = lme_los$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p5 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_los %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_los %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_los %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p6 <- ggplot(
  data = data_los %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = los)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = los_fit,
    aes(x = year_new, y = los_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\n100% data completeness",
    "\ntemporal trends of los",
    "\n10/17 extended",
    "\nslope = 0.3130",
    "\np-value = 0.1786"))


pdf(
  "output/figures/p_map_scatter_season_cumsum.pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
dev.off()



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
  ungroup()
view(data_sas %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_sas <- lme(
  sas ~ year_new,
  data = data_sas
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sas)
sas_fit <- data_sas %>% 
  # filter(state != "PR") %>%
  mutate(
    sas_fit.fixed = lme_sas$fitted[, 1],
    sas_fit.random = lme_sas$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p1 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_sas %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "red", mid= "white", high = "blue", midpoint = 0) +
  geom_point(data = data_sas %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_sas %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p2 <- ggplot(
  data = data_sas %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = sas)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = sas_fit,
    aes(x = year_new, y = sas_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\nno requirement on data completeness",
    "\ntemporal trends of sas",
    "\n14/21 advanced",
    "\nslope = -1.4259",
    "\np-value = 0.0074"))



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
  ungroup()
view(data_eas %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_eas <- lme(
  eas ~ year_new,
  data = data_eas
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_eas)
eas_fit <- data_eas %>% 
  # filter(state != "PR") %>%
  mutate(
    eas_fit.fixed = lme_eas$fitted[, 1],
    eas_fit.random = lme_eas$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p3 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_eas %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "red", mid= "white", high = "blue", midpoint = 0) +
  geom_point(data = data_eas %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_eas %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p4 <- ggplot(
  data = data_eas %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = eas)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = eas_fit,
    aes(x = year_new, y = eas_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\nno requirement on data completeness",
    "\ntemporal trends of eas",
    "\n6/14 advanced",
    "\nslope = 0.0054",
    "\np-value = 0.9908"))

pdf(
  "output/figures/p_map_scatter_season_threshold.pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
dev.off()



## las
# filter data
data_las <- df_metrics %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= 0.7) %>% 
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
  filter(observ_pct_as >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()
view(data_las %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_las <- lme(
  las ~ year_new,
  data = data_las
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_las)
las_fit <- data_las %>% 
  # filter(state != "PR") %>%
  mutate(
    las_fit.fixed = lme_las$fitted[, 1],
    las_fit.random = lme_las$fitted[, 2]
  ) %>%
  as_tibble()

# map & scatterplot
p9 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_las %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_las %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_las %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p10 <- ggplot(
  data = data_las %>% 
    # filter(state != "PR") %>%
    mutate(n = as.character(n)),
  aes(x = year_new, y = las)
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = las_fit,
    aes(x = year_new, y = las_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ggtitle(paste0(
    "\n80% data completeness in AS",
    "\ntemporal trends of las",
    "\n8/11 extended",
    "\nslope = 1.3265",
    "\np-value = 0.2316"))
pdf(
  "output/figures/p_map_scatter_season_threshold.pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
grid.arrange(p7, p8, ncol = 2, widths = c(2, 1))
grid.arrange(p9, p10, ncol = 2, widths = c(2, 1))
dev.off()



## allergy season integral
# filter data
data_integral_as <- df_metrics %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= 1) %>%
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
  filter(observ_pct_as >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, intercept, slope, r_squared, p_value) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()
view(data_integral_as %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n, p_value, slope))

# fit lme
lme_integral_as <- lme(
  log(integral_as) ~ year_new,
  # log(integral_as) ~ year_new,
  data = data_integral_as
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_integral_as)
integral_as_fit <- data_integral_as %>% 
  # filter(state != "PR") %>%
  mutate(
    integral_as_fit.fixed = lme_integral_as$fitted[, 1],
    integral_as_fit.random = lme_integral_as$fitted[, 2]
  ) %>%
  as_tibble()

# # check residuals
# df_residuals <- residuals(lme_integral_as) %>% 
#   data.frame() %>% 
#   rename("residual" = ".")
# p17 <- ggplot(df_residuals, aes(x = residual)) +
#   geom_density(color = "black") +
#   labs(x = "Residuals", y = "Density") +
#   ggtitle(paste0("data completeness: 80%", "\nlog(integral_as) ~ year")) +
#   theme_classic()
# p18 <- ggplot(df_residuals, aes(sample = residual)) +
#   geom_qq() +
#   geom_qq_line(color = "red") +
#   labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
#   ggtitle(paste0("slope = 0.013964", "\np-value = 0.3185")) +
#   # ggtitle("QQ Plot of Residuals") +
#   theme_classic()
# pdf(
#   "output/figures/p_residuals_integral_as.pdf",
#   width = 10,
#   height = 10*0.618
# )
# grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
# grid.arrange(p7, p9, p11, p8, p10, p12, ncol = 3)
# grid.arrange(p13, p15, p17, p14, p16, p18, ncol = 3)
# dev.off()

# map & scatterplot
p5 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = data_integral_as %>% mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3)), 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  # geom_point(data = data_peak %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(2, 5)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_integral_as %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_integral_as %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black")
p2 <- ggplot(
  data = data_integral_as %>% 
    mutate(n = as.character(n)),
  aes(x = year_new, y = log(integral_as))
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = integral_as_fit,
    aes(x = year_new, y = integral_as_fit.fixed),
    col = "black", linewidth = 1
  ) +
  xlab("year") +
  ylab("log(allergy season integral)") +
  ggtitle(paste0(
    "\n90% data completeness in AS",
    "\ntemporal trends of log(AS integral)",
    "\n6/11 increased",
    "\nslope = 0.0074058",
    "\np-value = 0.6504"))
pdf(
  "output/figures/p_map_scatter_log(integral_as).pdf",
  width = 12,
  height = 6
)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
grid.arrange(p3, p4, ncol = 2, widths = c(2, 1))
grid.arrange(p5, p6, ncol = 2, widths = c(2, 1))
dev.off()
