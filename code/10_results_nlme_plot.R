parameters_df <- read_rds("H:/phenology/phenology_spore/processed/metrics_with_climate.rds")
df_smooth <- read_rds("H:/phenology/phenology_spore/processed/fill_smooth_to2021.rds")
pacman::p_load(lme4)
pacman::p_load(blme)
pacman::p_load(nlme)
pacman::p_load(ggeffects)

# log(peak_con+1)~year
## read the data
# old dataset
# data_peak <- parameters_df %>%
#   filter((id == 5 & year %in% c(2013, 2016, 2017)) |
#     (id == 43 & year %in% c(2012, 2013, 2014)) |
#     (id == 21 & year != 2018) |
#     (id == 36) |
#     (id == 32) |
#     (id == 19 & year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) |
#     (id == 17) |
#     (id == 35 & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) |
#     (id == 41) |
#     (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016, 2017)) |
#     (id == 49) |
#     (id == 37) |
#     (id == 48 & year %in% c(2009, 2010, 2011))) %>%
#   mutate(id = as.character(id)) %>%
#   mutate(log_peak = log(peak + 1))
#new dataset (all total counts included, 2007-2021)
data_peak <- parameters_df %>%
  filter(
  (id == 3 & year %in% c(2013, 2016, 2017, 2018, 2020)) |
    (id == 4 & year %in% c(2012, 2013, 2014, 2018, 2019, 2020)) |
    (id == 10 & year != 2018) |
    (id == 11) |
    (id == 17) |
    (id == 20 & year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) |
    (id == 21 & year != 2021) |
    (id == 25) |
    (id == 28 & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020)) |
    (id == 29 & year %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) |
    (id == 31) |
    (id == 35 & year != 2013) |
    (id == 36 & year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) |
    (id == 42 & year != 2021) |
    (id == 44 & year != 2021) |
    (id == 46 & year %in% c(2009, 2010, 2011)) |
    (id == 48 & year %in% c(2010, 2013, 2015, 2017, 2018)) |
    (id == 42 & year != 2021)) %>%
  mutate(id = as.character(id)) %>%
  mutate(log_peak = log(peak + 1))
# model_lmer1 <- lmer(log_peak ~ year + (1 | id), data = data_peak)
# summary(model_lmer1)
# model_lmer2 <- lmer(log_peak ~ year + (year | id), data = data_peak, REML = TRUE)
# summary(model_lmer2)
# model_nlme1 <- lme(log_peak ~ year, data = data_peak, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
nlme_log_peak <- lme(log_peak ~ year, data = data_peak, random = ~ year | id, control = lmeControl(opt = "optim"))
summary(nlme_log_peak)
# nlme_log_peak_climate <- lme(log_peak ~ mat + tap + mvp, data = data_peak, random = ~ 1 | id)
# summary(nlme_log_peak_climate)
## read fitted data and variance
log_peak_fit <- data_peak %>%
  mutate(
    log_peak_fit.fixed = nlme_log_peak$fitted[, 1],
    log_peak_fit.random = nlme_log_peak$fitted[, 2]
  ) %>%
  as_tibble()
log_peak_fix_var <- ggpredict(nlme_log_peak, "year", type = "re") %>%
  as_tibble()
## plot
plot_log_peak_year <- ggplot() +
  geom_jitter(data = log_peak_fit, aes(x = year, y = log_peak, group = id, col = id)) +
  geom_path(data = log_peak_fit, aes(x = year, y = log_peak_fit.random, group = id, col = id)) +
  geom_path(data = log_peak_fit, aes(x = year, y = log_peak_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = log_peak_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 1)) +
  theme_classic()
ggsave(
  plot = plot_log_peak_year,
  filename = "~/spore_phenology/output/figures/plot_log_peak_year.pdf",
  width = 10,
  height = 10
)

# peak_doy~year
# model_lmer1 <- lmer(peak_doy ~ year + (1 | id), data = data_peak)
# model_nlme <- lme(log_con ~ year, data = data_peak, random = ~1 | id)
# summary(model_lmer1)
# model_nlme1 <- lme(peak_doy ~ year, data = data_peak, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
nlme_peak_doy <- lme(peak_doy ~ year, data = data_peak, random = ~ year | id)
summary(nlme_peak_doy)
# nlme_peak_doy_climate <- lme(peak_doy ~ mat + tap + mvp, data = data_peak, random = ~ 1 | id)
# summary(nlme_peak_doy_climate)
## read fitted data and variance
peak_doy_fit <- data_peak %>%
  mutate(
    peak_doy_fit.fixed = nlme_peak_doy$fitted[, 1],
    peak_doy_fit.random = nlme_peak_doy$fitted[, 2]
  ) %>%
  as_tibble()
peak_doy_fix_var <- ggpredict(nlme_peak_doy, "year", type = "re") %>%
  as_tibble()
## plot
plot_peak_doy <- ggplot() +
  geom_jitter(data = peak_doy_fit, aes(x = year, y = peak_doy, group = id, col = id)) +
  geom_path(data = peak_doy_fit, aes(x = year, y = peak_doy_fit.random, group = id, col = id)) +
  geom_path(data = peak_doy_fit, aes(x = year, y = peak_doy_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = peak_doy_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 1)) +
  theme_classic()
ggsave(
  plot = plot_peak_doy,
  filename = "~/spore_phenology/output/figures/plot_peak_doy.pdf",
  width = 10,
  height = 10
)

# log(integral+1)~year
## read the data
# ### old dataset
# data_integral <- parameters_df %>%
#   drop_na(integral) %>% 
#   filter(integral > 0) %>% 
#   mutate(log_integral = log(integral + 1)) %>%
#   mutate(id = as.character(id))
### new dataset
data_integral <- df_smooth %>%
  group_by(location, id, year) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  drop_na(count_smooth) %>%
  filter(count_smooth > 5) %>% 
  summarise(observ_percent = n() / 214) %>%
  full_join(parameters_df, by = c("location" = "location", "id" = "id", "year" = "year")) %>%
  drop_na(integral) %>%
  filter(observ_percent >= 0.5) %>% 
  mutate(log_integral = log(integral + 1)) %>%
  mutate(id = as.character(id)) %>% 
  ungroup()
# model_lmer1 <- lmer(log_integral ~ year + (1 | id), data = data_integral)
# summary(model_lmer1)
# model_nlme1 <- lme(log_integral ~ year, data = data_integral, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
nlme_integral <- lme(integral ~ year, data = data_integral, random = ~ year | location)
summary(nlme_integral)
## read fitted data and variance
integral_fit <- data_integral %>%
  ungroup() %>% 
  mutate(
    integral_fit.fixed = nlme_integral$fitted[, 1],
    integral_fit.random = nlme_integral$fitted[, 2]
  ) %>%
  as_tibble()
integral_fix_var <- ggpredict(nlme_integral, terms = c("year", "location"), type = "re") %>%
  as_tibble()
## plot
plot_integral <- ggplot() +
  geom_jitter(data = integral_fit, aes(x = year, y = integral, group = location, col = location)) +
  geom_path(data = integral_fit, aes(x = year, y = integral_fit.random, group = location, col = location)) +
  geom_path(data = integral_fit, aes(x = year, y = integral_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = integral_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2007, 2021, by = 3)) +
  theme_classic() +
  ylab("Seasonal Integral in Apr-Oct") +
  xlab("Year") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = function(x) x/1e+7) +
  ylab(expression(paste("Seasonal Integral in Apr-Oct (*10"^7*")")))
ggsave(
  plot = plot_integral,
  filename = "~/spore_phenology/output/figures/plot_integral.pdf",
  width = 4,
  height = 3
)
## fit nlme integral ~ climate
nlme_log_integral_climate <- lme(integral ~ tap, data = data_integral, random = ~ 1 | location)
summary(nlme_log_integral_climate)
## read fitted data and variance
integral_fit <- data_integral %>%
  ungroup() %>% 
  mutate(
    integral_fit.fixed = nlme_log_integral_climate$fitted[, 1],
    integral_fit.random = nlme_log_integral_climate$fitted[, 2]
  ) %>%
  as_tibble()
integral_fix_var <- ggpredict(nlme_log_integral_climate, terms = c("tap", "location"), type = "re") %>%
  as_tibble()
## plot
plot_integral_climate <- ggplot() +
  geom_jitter(data = integral_fit, aes(x = tap, y = integral, group = location, col = location)) +
  geom_path(data = integral_fit, aes(x = tap, y = integral_fit.random, group = location, col = location)) +
  geom_path(data = integral_fit, aes(x = tap, y = integral_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = integral_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  theme_classic() +
  ylab("integral Apr-Oct")
ggsave(
  plot = plot_integral_climate,
  filename = "~/spore_phenology/output/figures/plot_integral_climate.pdf",
  width = 15,
  height = 10
)
# start_doy~year
## read the data
# ### old dataset
# data_season <- parameters_df %>%
#   filter((id == 21 & year %in% c(2014, 2015, 2016, 2017)) |
#     (id == 36 & year != 2018) |
#     (id == 32) |
#     (id == 35 & year != 2013 & year != 2019) |
#     (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016)) |
#     (id == 49) |
#     (id == 37)) %>%
#   mutate(start_doy = format(start_date, "%j") %>% as.integer()) %>%
#   mutate(end_doy = format(end_date, "%j") %>% as.integer()) %>%
#   mutate(duration = end_doy - start_doy + 1) %>%
#   mutate(id = as.character(id))
### new dataset
data_season <- parameters_df %>%
  filter(
    (id == 4 & year %in% c(2018, 2019, 2020)) |
      (id == 10 & year %in% c(2009, 2010, 2014, 2015, 2016, 2017)) |
      (id == 11 & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015)) |
      (id == 17) |
      (id == 20 & year %in% c(2015, 2017, 2018, 2019, 2020)) |
      (id == 28 & year != 2013 & year != 2019 & year != 2021) |
      (id == 36 & year %in% c(2012, 2014, 2015, 2016, 2018, 2020)) |
      (id == 42 & year != 2011 & year != 2016 & year != 2019 & year != 2021) |
      (id == 44 & year != 2021)) %>% 
  mutate(id = as.character(id))
# model_lmer1 <- lmer(start_doy ~ year + (1 | id), data = data_season)
# summary(model_lmer1)
# model_nlme1 <- lme(start_doy ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
# nlme_start_doy <- lme(sos ~ year, data = data_season, random = ~ year | id)
# summary(nlme_start_doy)
# ## read fitted data and variance
# start_doy_fit <- data_season %>%
#   mutate(
#     start_doy_fit.fixed = nlme_start_doy$fitted[, 1],
#     start_doy_fit.random = nlme_start_doy$fitted[, 2]
#   ) %>%
#   as_tibble()
# start_doy_fix_var <- ggpredict(nlme_start_doy, "year", type = "re") %>%
#   as_tibble()
# ## plot
# plot_start_doy <- ggplot() +
#   geom_jitter(data = start_doy_fit, aes(x = year, y = start_doy, group = id, col = id)) +
#   geom_path(data = start_doy_fit, aes(x = year, y = start_doy_fit.random, group = id, col = id)) +
#   geom_path(data = start_doy_fit, aes(x = year, y = start_doy_fit.fixed), col = "black", linewidth = 1) +
#   geom_ribbon(data = start_doy_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
#   scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
#   theme_classic()
# ggsave(
#   plot = plot_start_doy,
#   filename = "~/spore_phenology/output/figures/plot_start_doy.pdf",
#   width = 10,
#   height = 10
# )
nlme_start_doy_climate <- lme(sos ~ mat + tap + mvp, data = data_season, random = ~ 1 | id)
summary(nlme_start_doy_climate)
## read fitted data and variance
sos_fit <- data_season %>%
  ungroup() %>% 
  mutate(
    sos_fit.fixed = nlme_start_doy_climate$fitted[, 1],
    sos_fit.random = nlme_start_doy_climate$fitted[, 2]
  ) %>%
  as_tibble()
sos_fix_var <- ggpredict(nlme_start_doy_climate, "tap", type = "re") %>%
  as_tibble()
## plot
plot_start_doy <- ggplot() +
  geom_jitter(data = sos_fit, aes(x = tap, y = sos, group = location, col = location)) +
  geom_path(data = sos_fit, aes(x = tap, y = sos_fit.random, group = location, col = location)) +
  geom_path(data = sos_fit, aes(x = tap, y = sos_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = sos_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2007, 2021, by = 1)) +
  theme_classic()
ggsave(
  plot = plot_start_doy,
  filename = "~/spore_phenology/output/figures/plot_start_doy.pdf",
  width = 10,
  height = 10
)

# end_doy~year
# model_lmer1 <- lmer(end_doy ~ year + (1 | id), data = data_season)
# model_nlme1 <- lme(end_doy ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
nlme_end_doy <- lme(eos ~ year, data = data_season, random = ~ year | id, control = lmeControl(opt = "optim"))
summary(nlme_end_doy)
## read fitted data and variance
end_doy_fit <- data_season %>%
  mutate(
    end_doy_fit.fixed = nlme_end_doy$fitted[, 1],
    end_doy_fit.random = nlme_end_doy$fitted[, 2]
  ) %>%
  as_tibble()
end_doy_fix_var <- ggpredict(nlme_end_doy, "year", type = "re") %>%
  as_tibble()
## plot
plot_end_doy <- ggplot() +
  geom_jitter(data = end_doy_fit, aes(x = year, y = end_doy, group = id, col = id)) +
  geom_path(data = end_doy_fit, aes(x = year, y = end_doy_fit.random, group = id, col = id)) +
  geom_path(data = end_doy_fit, aes(x = year, y = end_doy_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = end_doy_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
  theme_classic()
ggsave(
  plot = plot_end_doy,
  filename = "~/spore_phenology/output/figures/plot_end_doy.pdf",
  width = 10,
  height = 10
)

# duration~year
# model_lmer1 <- lmer(duration ~ year + (1 | id), data = data_season)
# summary(model_lmer1)
# model_nlme1 <- lme(duration ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
## fit nlme
nlme_duration <- lme(los ~ year, data = data_season, random = ~ year | id)
summary(nlme_duration)
## read fitted data and variance
duration_fit <- data_season %>%
  mutate(
    duration_fit.fixed = nlme_duration$fitted[, 1],
    duration_fit.random = nlme_duration$fitted[, 2]
  ) %>%
  as_tibble()
duration_fix_var <- ggpredict(nlme_duration, "year", type = "re") %>%
  as_tibble()
## plot
plot_duration <- ggplot() +
  geom_jitter(data = duration_fit, aes(x = year, y = duration, group = id, col = id)) +
  geom_path(data = duration_fit, aes(x = year, y = duration_fit.random, group = id, col = id)) +
  geom_path(data = duration_fit, aes(x = year, y = duration_fit.fixed), col = "black", linewidth = 1) +
  geom_ribbon(data = duration_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
  theme_classic()
ggsave(
  plot = plot_duration,
  filename = "~/spore_phenology/output/figures/plot_duration.pdf",
  width = 10,
  height = 10
)





















