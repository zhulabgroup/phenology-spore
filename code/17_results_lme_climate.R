df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_daymet.rds"))

## peak
data_peak <- df_metrics %>% 
  filter(peak_check == 1) %>%
  drop_na(peak) %>% 
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
  filter(peak_check == 1) %>%
  drop_na(peak) %>% 
  filter(observ_pct >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup() %>% 
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
  ungroup()
# view(data_peak %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

lm_peak <- lm(
  slope ~ slope_tap,
  # peak_doy ~ mat + tap,
  data = data_peak
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lm_peak)
p4 <- ggplot(
  data = data_peak,
  aes(x = slope_tap, y = slope)
) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1)
pdf(
  "output/figures/p_scatter_trend_log(peak)_tap.pdf",
  width = 8,
  height = 8 * .618
)
p1
p2
p3
p4
dev.off()

# fit lme
# lme_climate_peak <- lme(
#   log(peak) ~ mat + tap,
#   # peak_doy ~ mat + tap,
#   data = data_peak,
#   random = ~ 1 | n
#   # ,
#   # control = lmeControl(opt = "optim")
# )
# summary(lme_climate_peak)
# peak_fit <- data_peak %>% 
  # mutate(
  #   peak_fit.fixed = lme_climate_peak$fitted[, 1],
  #   peak_fit.random = lme_climate_peak$fitted[, 2]
  # ) %>%
  # as_tibble()
# fixed_effects <- fixef(lme_climate_peak)
# peak_fit$predict <- fixed_effects["tap"] * peak_fit$tap + fixed_effects["(Intercept)"]

# p4 <- ggplot(
#   data = data_peak %>% mutate(n = as.character(n)),
#   aes(x = tap, y = log(peak))
# ) +
#   geom_point(aes(color = n), size = 0.5) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
#   geom_line(data = peak_fit, aes(y = predict), color = "red", linewidth = 1)
#   # geom_path(
#   #   data = peak_fit,
#   #   aes(x = tap, y = peak_fit.fixed),
#   #   col = "black", linewidth = 1
#   # )
# pdf(
#   "output/figures/p_scatter_log(peak)_tap.pdf",
#   width = 12,
#   height = 6
# )
# p1
# p2
# p3
# p4
# dev.off()


## integral
data_integral <- df_metrics %>% 
  drop_na(integral) %>% 
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
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup() %>% 
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
  ungroup()
# view(data_integral %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))
lm_integral <- lm(slope ~ slope_tap, data = data_integral)
summary(lm_integral)
p4 <- ggplot(
  data = data_integral,
  aes(x = slope_tap, y = slope)
) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1)
pdf(
  "output/figures/p_scatter_trend_log(integral)_tap.pdf",
  width = 8,
  height = 8 * .618
)
p1
p2
p3
p4
dev.off()
# # fit lme
# lme_climate_integral <- lme(
#   # integral ~ mat * tap,
#   log(integral) ~ mat + tap,
#   data = data_integral,
#   random = ~ 1 | n
#   # ,
#   # control = lmeControl(opt = "optim")
# )
# summary(lme_climate_integral)
# integral_fit <- data_integral %>% 
#   mutate(
#     integral_fit.fixed = lme_climate_integral$fitted[, 1],
#     integral_fit.random = lme_climate_integral$fitted[, 2]
#   ) %>%
#   as_tibble()
# fixed_effects <- fixef(lme_climate_integral)
# integral_fit$predict <- fixed_effects["tap"] * integral_fit$tap + fixed_effects["(Intercept)"]
# 
# p4 <- ggplot(
#   data = data_integral %>% mutate(n = as.character(n)),
#   aes(x = tap, y = log(integral))
# ) +
#   geom_point(aes(color = n), size = 0.5) +
#   theme_classic() +
#   # theme(legend.position = "none") +
#   # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
#   geom_line(data = integral_fit, aes(y = predict), color = "red", linewidth = 1)
# # geom_path(
# #   data = peak_fit,
# #   aes(x = tap, y = peak_fit.fixed),
# #   col = "black", linewidth = 1
# # )
# pdf(
#   "output/figures/p_scatter_log(integral)_tap.pdf",
#   width = 12,
#   height = 6
# )
# p1
# p2
# p3
# p4
# dev.off()




## sos
data_sos <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sos %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_climate_sos <- lme(
  los ~ mat + tap,
  data = data_sos,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_climate_sos)





## sas
data_sas <- df_metrics %>% 
  drop_na(eas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sas %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_sas <- lme(
  eas ~ mat + tap,
  data = data_sas,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sas)
## las
data_las <- df_metrics %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_las %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_las <- lme(
  las ~ mat + tap,
  data = data_las,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_las)





## as integral
data_integral_as <- df_metrics %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= 0.8) %>%
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
  filter(observ_pct_as >= 0.8) %>%
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>%
  ungroup() %>% 
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
  ungroup()
# view(data_integral_as %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))
lm_integral_as <- lm(slope ~ slope_tap, data = data_integral_as)
summary(lm_integral_as)
p3 <- ggplot(
  data = data_integral_as,
  aes(x = slope_tap, y = slope)
) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1)
pdf(
  "output/figures/p_scatter_trend_log(integral_as)_tap.pdf",
  width = 8,
  height = 8 * .618
)
p1
p2
p3
dev.off()

# # fit lme
# lme_integral_as <- lme(
#   log(integral_as) ~ mat + tap,
#   data = data_integral_as,
#   random = ~ 1 | n
#   # ,
#   # control = lmeControl(opt = "optim")
# )
# summary(lme_integral_as)
# integral_fit <- data_integral_as %>% 
#   mutate(
#     integral_fit.fixed = lme_integral_as$fitted[, 1],
#     integral_fit.random = lme_integral_as$fitted[, 2]
#   ) %>%
#   as_tibble()
# fixed_effects <- fixef(lme_integral_as)
# integral_fit$predict <- fixed_effects["tap"] * integral_fit$tap + fixed_effects["(Intercept)"]
# 
# p3 <- ggplot(
#   data = data_integral_as %>% mutate(n = as.character(n)),
#   aes(x = tap, y = log(integral))
# ) +
#   geom_point(aes(color = n), size = 0.5) +
#   theme_classic() +
#   # theme(legend.position = "none") +
#   # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
#   geom_line(data = integral_fit, aes(y = predict), color = "red", linewidth = 1)
# # geom_path(
# #   data = peak_fit,
# #   aes(x = tap, y = peak_fit.fixed),
# #   col = "black", linewidth = 1
# # )
# pdf(
#   "output/figures/p_scatter_log(integral as)_tap.pdf",
#   width = 12,
#   height = 6
# )
# p1
# p2
# p3
# dev.off()





