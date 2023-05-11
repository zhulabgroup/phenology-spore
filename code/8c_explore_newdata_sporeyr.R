df_smooth_sporeyr <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_sporeyr.rds"))
df_metrics_sporeyr <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_sporeyr.rds"))
df_smooth_sporeyr <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_spore/processed/2023-04-25/fill_smooth_sporeyr.rds")
df_metrics_sporeyr <- read_rds("/nfs/turbo/seas-zhukai/phenology/phenology_spore/processed/2023-04-25/metrics_sporeyr.rds")
# seasonal integral
## select data
data_integral <- df_smooth_sporeyr %>%
  drop_na(count_smooth) %>% 
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  group_by(lat, lon, city, state, country, n, sporeyr) %>%
  summarise(observ_percent = n() / 214) %>%
  full_join(df_metrics_sporeyr, by = c("lat", "lon", "city", "state", "country", "n", "sporeyr")) %>%
  filter(integral > 0) %>%
  filter(observ_percent >= 0.5)

# peak concentration and date
ggplot(data = df_smooth_sporeyr %>% filter(n == 44)) +
  geom_line(aes(x = sporeyr_date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(n == 44) %>% mutate(peak_date = as.POSIXct(as.Date(peak_doy, origin = paste0(sporeyr, "-01-01"))) -1))$peak_date, color = "gray")
44  46  48  49  51  52  54  55  56  57  60  61  64  66  67  68  69  71  74  78  79  81
82  83  84  85  86  88  89  92  93  95  96 105

## select data
data_peak <- df_metrics_sporeyr %>%
  filter(
    (n == 1 & sporeyr != 2015 & sporeyr != 2019 & sporeyr != 2020) |
      (n == 2 & sporeyr %in% c(2015, 2016, 2017, 2018)) |
      (n == 4 & sporeyr %in% c(2003, 2004, 2005, 2006, 2007, 2009, 2010)) |
      (n == 5 & sporeyr %in% c(2004, 2005, 2006, 2007, 2009, 2010, 2011)) |
      (n == 6 & sporeyr != 2010) |
      (n == 8 & sporeyr != 2008 & sporeyr != 2009 & sporeyr != 2010) |
      (n == 9 & sporeyr %in% c(2005, 2006, 2009)) |
      (n == 15 & sporeyr != 2007 & sporeyr != 2009 & sporeyr != 2020 & sporeyr != 2021 & sporeyr != 2022) |
      (n == 20 & sporeyr != 2003 & sporeyr != 2004 & sporeyr != 2010 & sporeyr != 2012 & sporeyr != 2013) |
      (n == 27 & sporeyr %in% c(2008, 2009, 2010)) |
      (n == 35 & sporeyr != 2002) |
      (n == 36 & sporeyr != 2006 & sporeyr != 2010) |
      (n == 42 & sporeyr != 2002 & sporeyr != 2016 & sporeyr != 2017 & sporeyr != 2018 & sporeyr != 2019 & sporeyr != 2020) |
      (n == 44 & sporeyr != 2014 & sporeyr != 2022) |
      
    )%>%
  dplyr::select(lat, lon, location, id, sporeyr, peak, peak_doy)
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





# spore season
ggplot(data = df_smooth_sporeyr %>% filter(n == 105)) +
  geom_line(aes(x = sporeyr_date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(n == 105) %>% mutate(sos_date = as.POSIXct(as.Date(sos, origin = paste0(sporeyr, "-01-01"))) -1))$sos_date, color = "red") +
  geom_vline(xintercept = (df_metrics_sporeyr %>% filter(n == 105) %>% mutate(eos_date = as.POSIXct(as.Date(eos, origin = paste0(sporeyr, "-01-01"))) -1))$eos_date, color = "blue")

## select data
data_season <- df_metrics_sporeyr %>%
  drop_na(sos) %>% 
  filter(
    (n == 1 & sporeyr %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)) |
      (n == 2 & sporeyr %in% c(2015, 2016, 2018)) |
      (n == 4 & sporeyr %in% c(2004, 2005, 2006, 2007, 2009, 2010)) |
      (n == 6 & sporeyr %in% c(2012, 2014, 2015, 2016, 2019, 2020, 2021)) |
      (n == 8 & sporeyr %in% c(2013, 2014, 2015, 2016, 2018, 2019, 2020)) |
      (n == 15 & sporeyr %in% c(2005, 2006, 2008, 2013, 2014, 2015, 2017, 2018)) |
      (n == 35 & sporeyr != 2002 & sporeyr != 2005 & sporeyr != 2007 & sporeyr != 2009 & sporeyr != 2011 & sporeyr != 2013) |
      (n == 36 & sporeyr %in% c(2004, 2008, 2012, 2013, 2014, 2016)) |
      (n == 42 & sporeyr != 2002 & sporeyr != 2011 & sporeyr != 2016 & sporeyr != 2017) |
      (n == 44 & sporeyr %in% c(2016, 2017, 2019, 2020, 2021)) |
      (n == 55 & sporeyr != 2003 & sporeyr != 2022) |
      (n == 60 & sporeyr != 2003 & sporeyr != 2005 & sporeyr != 2008 & sporeyr != 2009 & sporeyr != 2011 & sporeyr != 2022) |
      (n == 66 & sporeyr != 2016 & sporeyr != 2017 & sporeyr != 2018) |
      (n == 78 & sporeyr %in% c(2018, 2019, 2020)) |
      (n == 105 & sporeyr %in% c(2003, 2004, 2005, 2006)))
