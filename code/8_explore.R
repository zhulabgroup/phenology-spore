df_smooth <- read_rds(str_c(.path$dat_process, "fill_smooth_to2021.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "metrics_with_climate.rds"))

# peak concentration and date
# 29 stations
ggplot(data = df_smooth) +
  geom_line(aes(x = date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ylab("count") +
  geom_point(data = df_metrics %>% mutate(peak_date = as.POSIXct(as.Date(peak_doy, origin = paste0(year, "-01-01"))) -1), aes(x = peak_date, y = peak), size = 0.5, color = "red") +
  facet_wrap(. ~ location, ncol = 6, scales = "free_y")
# each station
ggplot(data = df_smooth %>% filter(location == "Dallas, TX")) +
  geom_line(aes(x = date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics %>% filter(location == "Dallas, TX") %>% mutate(peak_date = as.POSIXct(as.Date(peak_doy, origin = paste0(year, "-01-01"))) -1))$peak_date, color = "gray")
# select data
data_peak <- df_metrics %>%
  filter(
    (location == "Austin Area, TX" & year %in% c(2013, 2016, 2017, 2018, 2020)) |
      (location == "Bellevue, NE" & year %in% c(2012, 2013, 2014, 2018, 2019, 2020)) |
      (location == "Dallas, TX" & year != 2018) |
      (location == "Dayton, OH") |
      (location == "Flower Mound, TX") |
      (location == "Houston 2, TX" & year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) |
      (location == "Kansas City, MO" & year != 2021) |
      (location == "Mount Laurel, NJ") |
      (location == "Oklahoma City, OK" & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020)) |
      (location == "Olean, NY" & year %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) |
      (location == "Philadelphia, PA") |
      (location == "San Antonio 3, TX" & year != 2013) |
      (location == "San Antonio 2, TX" & year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) |
      (location == "Silver Spring, MD" & year != 2021) |
      (location == "St. Louis, MO" & year != 2021) |
      (location == "Tampa, FL" & year %in% c(2009, 2010, 2011)) |
      (location == "Twin Falls, ID" & year %in% c(2010, 2013, 2015, 2017, 2018)) |
      (location == "Silver Spring, MD" & year != 2021)) %>%
  dplyr::select(location, id, year, peak, peak_doy)
# temporal trend of each station
## peak
ggplot(data = data_peak, aes(x = year, y = log(peak + 1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(5, 13) +
  facet_wrap(. ~ location * id, ncol = 5) +
  ggpubr::stat_cor(label.y = 12) +
  ggpubr::stat_regline_equation(label.y = 11)
## peak_doy
ggplot(data = data_peak, aes(x = year, y = peak_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(100, 366) +
  facet_wrap(. ~ location * id, ncol = 5) +
  ggpubr::stat_cor(method = "pearson", label.y = 350)

# integrals in Apr-Oct
# select data
data_integral <- df_smooth %>%
  group_by(location, id, year) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  drop_na(count_smooth) %>%
  filter(count_smooth > 5) %>% 
  summarise(observ_percent = n() / 214) %>%
  full_join(df_metrics, by = c("location" = "location", "id" = "id", "year" = "year")) %>%
  drop_na(integral) %>%
  filter(observ_percent >= 0.5)
# temporal trend of each station
ggplot(data = data_integral, aes(x = year, y = log(integral + 1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(10, 18) +
  facet_wrap(. ~ location * id, ncol = 6) +
  ggpubr::stat_cor(method = "pearson", label.y = 17)

# growing season start date & end date
# 
ggplot(data = df_smooth %>% filter(id == 42)) +
  geom_line(aes(x = date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics %>% filter(id == 42) %>% mutate(sos_date = as.POSIXct(as.Date(sos, origin = paste0(year, "-01-01"))) -1))$sos_date, color = "red") +
  geom_vline(xintercept = (df_metrics %>% filter(id == 42) %>% mutate(eos_date = as.POSIXct(as.Date(eos, origin = paste0(year, "-01-01"))) -1))$eos_date, color = "blue")
# select data
data_season <- df_metrics %>%
  filter(
    (location == "Bellevue, NE" & year %in% c(2018, 2019, 2020)) |
      (location == "Dallas, TX" & year %in% c(2009, 2010, 2014, 2015, 2016, 2017)) |
      (location == "Dayton, OH" & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015)) |
      (location == "Flower Mound, TX") |
      (location == "Houston 2, TX" & year %in% c(2015, 2017, 2018, 2019, 2020)) |
      (location == "Oklahoma City, OK" & year != 2013 & year != 2019 & year != 2021) |
      (location == "San Antonio 2, TX" & year %in% c(2012, 2014, 2015, 2016, 2018, 2020)) |
      (location == "Silver Spring, MD" & year != 2011 & year != 2016 & year != 2019 & year != 2021) |
      (location == "St. Louis, MO" & year != 2021))
# temporal trend of each station
## sos
ggplot(data = data_season, aes(x = year, y = sos)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 366) +
  facet_wrap(. ~ location * id, ncol = 3) +
  ggpubr::stat_cor(method = "pearson", label.y = 350)
## eos
ggplot(data = data_season, aes(x = year, y = eos)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 366) +
  facet_wrap(. ~ location * id, ncol = 3) +
  ggpubr::stat_cor(method = "pearson", label.y = 350)
## los
ggplot(data = data_season, aes(x = year, y = los)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 366) +
  facet_wrap(. ~ location * id, ncol = 3) +
  ggpubr::stat_cor(method = "pearson", label.y = 350)