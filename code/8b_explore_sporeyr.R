df_smooth_sporeyr <- read_rds(str_c(.path$dat_process, "fill_smooth_to2021_sporeyr.rds"))
df_metrics_sporeyr <- read_rds(str_c(.path$dat_process, "metrics_sporeyr.rds"))


# peak concentration and date
ggplot(data = df_smooth_sporeyr %>% filter(location == "Waterbury, CT")) +
  geom_line(aes(x = sporeyr_date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(location == "Waterbury, CT") %>% mutate(peak_date = as.POSIXct(as.Date(peak_doy, origin = paste0(sporeyr, "-01-01"))) -1))$peak_date, color = "gray")

## select data
data_peak <- df_metrics_sporeyr %>%
  filter(
    (location == "Austin Area, TX" & sporeyr %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2020)) |
      (location == "Bellevue, NE" & sporeyr %in% c(2012, 2013, 2014, 2018, 2019, 2020)) |
      (location == "Dallas, TX" & sporeyr != 2008 & sporeyr != 2018) |
      (location == "Dayton, OH" & sporeyr != 2008) |
      (location == "Flower Mound, TX" & sporeyr != 2008) |
      (location == "Houston 2, TX" & sporeyr %in% c(2011, 2012, 2013, 2014, 2015, 2018, 2019, 2020)) |
      (location == "Kansas City, MO" & sporeyr != 2021) |
      (location == "Mount Laurel, NJ" & sporeyr != 2014) |
      (location == "Oklahoma City, OK" & sporeyr %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020)) |
      (location == "Olean, NY" & sporeyr %in% c(2011, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) |
      (location == "Philadelphia, PA") |
      (location == "San Antonio 3, TX" & sporeyr != 2013) |
      (location == "San Antonio 2, TX" & sporeyr != 2011 & sporeyr != 2021) |
      (location == "Silver Spring, MD" & sporeyr != 2008 & sporeyr != 2021) |
      (location == "St. Louis, MO" & sporeyr != 2008 & sporeyr != 2021) |
      (location == "Tampa, FL" & sporeyr %in% c(2009, 2010, 2011)) |
      (location == "Tulsa, OK" & sporeyr %in% c(2009, 2012, 2013, 2014, 2016)) |
      (location == "Twin Falls, ID" & sporeyr %in% c(2010, 2011, 2013, 2015, 2017, 2018)) |
      (location == "Waterbury, CT" & sporeyr != 2011 & sporeyr != 2019 & sporeyr != 2021)) %>%
  dplyr::select(lat, lon, location, id, sporeyr, peak, peak_doy)


# seasonal integral
## select data
data_integral <- df_smooth_sporeyr %>%
  drop_na(count_smooth) %>% 
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  group_by(lat, lon, location, id, sporeyr) %>%
  summarise(observ_percent = n() / 214) %>%
  full_join(df_metrics_sporeyr, by = c("lat", "lon", "location", "id", "sporeyr")) %>%
  filter(integral > 0) %>%
  filter(observ_percent >= 0.5)


# spore season
ggplot(data = df_smooth_sporeyr %>% filter(location == "Waterbury, CT")) +
  geom_line(aes(x = sporeyr_date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(location == "Waterbury, CT") %>% mutate(sos_date = as.POSIXct(as.Date(sos, origin = paste0(sporeyr, "-01-01"))) -1))$sos_date, color = "red") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(location == "Waterbury, CT") %>% mutate(eos_date = as.POSIXct(as.Date(eos, origin = paste0(sporeyr, "-01-01"))) -1))$eos_date, color = "blue")

## select data
data_season <- df_metrics %>%
  filter(
    (location == "Bellevue, NE" & year %in% c(2018, 2019, 2020)) |
      (location == "Dallas, TX" & year %in% c(2012, 2014, 2015, 2016, 2017)) |
      (location == "Dayton, OH" & year %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015)) |
      (location == "Flower Mound, TX" & year != 2008 & year != 2013 & year != 2016) |
      (location == "Houston 2, TX" & year %in% c(2013, 2014, 2015, 2016, 2018, 2019, 2020)) |
      (location == "Oklahoma City, OK" & year %in% c(2010, 2012, 2014, 2015, 2016, 2018, 2020)) |
      (location == "San Antonio 2, TX" & year %in% c(2012, 2013, 2015, 2016, 2018, 2020)) |
      (location == "Silver Spring, MD" & year != 2008 & year != 2009 & year != 2011 & year != 2021) |
      (location == "St. Louis, MO" & year != 2008 & year != 2021))