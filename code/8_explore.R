parameters_df <- read_rds("~/spore_phenology/data/parameters.rds")
data_smooth_df <- read_rds("~/spore_phenology/data/data_smooth.rds")


# peak concentration and date
#
ggplot(data = data_smooth_df, aes(x = date, y = log(count_smooth + 1))) +
  scale_x_date(date_breaks = "1 year") +
  geom_point(data = parameters_df, aes(x = peak_date, y = log(peak_con + 1)), size = 0.5, color = "red") +
  geom_path() +
  facet_wrap(. ~ location * id, ncol = 6)
#
ggplot(data = data_smooth_df %>% filter(id == 18), aes(x = date, y = log(count_smooth + 1))) +
  scale_x_date(date_breaks = "1 year") +
  geom_vline(xintercept = (parameters_df %>% filter(id == 18))$peak_date, color = "gray") +
  geom_path() +
  facet_wrap(. ~ location * id, ncol = 6)
# select data
data_peak <- parameters_df %>%
  filter((id == 5 & year %in% c(2013, 2016, 2017)) |
    (id == 43 & year %in% c(2012, 2013, 2014)) |
    (id == 21 & year != 2018) |
    (id == 36) |
    (id == 32) |
    (id == 19 & year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) |
    (id == 17) |
    (id == 35 & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) |
    (id == 41) |
    (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016, 2017)) |
    (id == 49) |
    (id == 37) |
    (id == 48 & year %in% c(2009, 2010, 2011))) %>%
  dplyr::select(location, id, year, peak_con, peak_date)
# plot
ggplot(data = data_peak, aes(x = year, y = log(peak_con + 1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(5, 13) +
  facet_wrap(. ~ location * id, ncol = 5) +
  stat_cor(label.y = 12) +
  stat_regline_equation(label.y = 11)

ggplot(data = data_peak %>% mutate(peak_doy = format(peak_date, "%j") %>% as.integer()), aes(x = year, y = peak_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(100, 366) +
  facet_wrap(. ~ location * id, ncol = 5) +
  stat_cor(method = "pearson", label.y = 350)


# integrals in time window
# trend of measurements in time window
ggplot(
  data = data_smooth_df %>%
    group_by(location, id, year) %>%
    filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
    filter(!is.na(count_smooth)) %>%
    summarise(observ_percent = n() / 214),
  aes(x = year, y = observ_percent)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  facet_wrap(. ~ location * id, ncol = 6) +
  stat_cor(method = "pearson", label.y = 500)
# select data
data_integrals <- data_smooth_df %>%
  group_by(location, id, year) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  filter(!is.na(count_smooth)) %>%
  summarise(observ_percent = n() / 214) %>%
  full_join(parameters_df, by = c("location" = "location", "id" = "id", "year" = "year")) %>%
  filter(!is.na(integral)) %>%
  filter(observ_percent >= 0.5)
# plot
ggplot(data = data_integrals, aes(x = year, y = log(integral + 1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(10, 18) +
  facet_wrap(. ~ location * id, ncol = 6) +
  stat_cor(method = "pearson", label.y = 17)


# growing season start date & end date
#
ggplot(data = data_smooth_df %>% filter(id == 35), aes(x = date, y = log(count_smooth + 1))) +
  scale_x_date(date_breaks = "1 year") +
  geom_vline(xintercept = (parameters_df %>% filter(id == 35))$start_date, color = "red") +
  geom_vline(xintercept = (parameters_df %>% filter(id == 35))$end_date, color = "blue") +
  geom_path() +
  facet_wrap(. ~ location * id, ncol = 6)
#
data_season <- parameters_df %>%
  filter((id == 21 & year %in% c(2014, 2015, 2016, 2017)) |
    (id == 36 & year != 2018) |
    (id == 32) |
    (id == 35 & year != 2013 & year != 2019) |
    (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016)) |
    (id == 49) |
    (id == 37))
#
ggplot(data = data_season %>% mutate(start_doy = format(start_date, "%j") %>% as.integer()), aes(x = year, y = start_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 366) +
  facet_wrap(. ~ location * id, ncol = 4) +
  stat_cor(method = "pearson", label.y = 350)
ggplot(data = data_season %>% mutate(end_doy = format(end_date, "%j") %>% as.integer()), aes(x = year, y = end_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 366) +
  facet_wrap(. ~ location * id, ncol = 4) +
  stat_cor(method = "pearson", label.y = 350)
