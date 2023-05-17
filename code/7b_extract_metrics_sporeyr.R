# df_smooth <- read_rds(str_c(.path$dat_process, "fill_smooth_to2021.rds"))
# 
# # determine the lag day: 40
# df_smooth_id <- df_smooth %>% 
#   distinct(id, .keep_all = T) %>% 
#   dplyr::select(id) %>% 
#   mutate(n = row_number())
# df_smooth <- df_smooth %>% 
#   left_join(df_smooth_id, by = "id")
# 
# ggplot(data = df_smooth %>%
#          filter(count_smooth > 5) %>%
#          filter(n == 20))+
#   geom_line(aes(x = doy, y = count_smooth, group = year, col = year)) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_continuous(breaks = seq(0, 366, by = 10))
# 
# # generate new df_smooth
# df_smooth_sporeyr <- df_smooth %>%
#   mutate(sporeyr = case_when (doy <= 40 ~ (year-1),
#                               TRUE ~ as.double(year))) %>%
#   mutate(sporeyr_doy = case_when(doy > 40 ~ (doy - 40),
#                                  TRUE ~ as.numeric(difftime(date, as.Date(paste0(sporeyr,"-01-01")) ) - 40 + 1))) %>%
#   mutate(sporeyr_date = as.POSIXct(as.Date(sporeyr_doy, origin = paste0(sporeyr, "-01-01"))) -1)
# write_rds(df_smooth_sporeyr, "/nfs/turbo/seas-zhukai/phenology/phenology_spore/processed/2023-04-25/fill_smooth_sporeyr.rds")

# peak concentration & date
df_peak_sporeyr <- df_smooth_sporeyr %>%
  drop_na(count_smooth) %>%
  group_by(lat, lon, city, state, country, n, sporeyr) %>%
  filter(count_smooth == max(count_smooth)) %>%
  summarise(
    peak = head(count_smooth, 1),
    peak_doy = head(sporeyr_doy, 1)
  )

# integral in time window
df_integral_sporeyr <- df_smooth_sporeyr %>%
  drop_na(count_smooth) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  group_by(lat, lon, city, state, country, n, sporeyr) %>%
  summarise(integral = sum(count_smooth) / n() * 214)

# growing season
df_season_sporeyr <- df_smooth_sporeyr %>%
  drop_na(count_smooth) %>%
  filter(count_smooth > 0) %>%
  group_by(lat, lon, city, state, country, n, sporeyr) %>%
  filter(count_smooth >= quantile(count_smooth, 0.3, na.rm = T)) %>%
  summarise(
    sos = min(sporeyr_doy),
    eos = max(sporeyr_doy)
  ) %>%
  mutate(los = eos - sos)

df_metrics_sporeyr <- list(df_peak_sporeyr, df_integral_sporeyr, df_season_sporeyr) %>% reduce(full_join, by = c("lat", "lon", "city", "state", "country", "n", "sporeyr"))
write_rds(df_metrics_sporeyr, "/nfs/turbo/seas-zhukai/phenology/phenology_spore/processed/2023-04-25/metrics_sporeyr.rds")
