# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))
df_daymet <- read_rds(str_c(.path$dat_process, "2023-04-25/smooth_daymet.rds"))


# # acquire data
# meta <- df_metrics %>%
#   ungroup() %>% 
#   distinct(id, .keep_all = TRUE) %>% 
#   filter(country == "US") %>% 
#   dplyr::select(n, lat, lon)
# write.table(meta, str_c(.path$dat_process, "2023-04-25/meta.rds"), 
#             sep = ",",
#             col.names = TRUE,
#             row.names = FALSE,
#             quote = FALSE)
# tidyweather  <- function(x) {
#   # pull out the data frame
#   # then pull daymet data
#   datadf  <- pluck(x, "data")
#   datadf$site       <- x[[1]]
#   datadf$title      <- x[[2]]
#   datadf$latitude   <- x[[3]]
#   datadf$longitude  <- x[[4]]
#   datadf$altitude   <- x[[5]]
#   datadf$tile1      <- x[[6]]
#   return(datadf)
# }
# df_daymet <- daymetr::download_daymet_batch(file_location = str_c(.path$dat_process, "2023-04-25/meta.rds"),
#                                             start = 2003,
#                                             end = 2022,
#                                             internal = TRUE) %>% 
#   map_df(tidyweather)
# df_daymet <- df_daymet %>% 
#   rename(
#     doy = yday,
#     prcp = prcp..mm.day.,
#     tmax = tmax..deg.c.,
#     tmin = tmin..deg.c.,
#     vp = vp..Pa.,
#     lat = latitude,
#     lon = longitude,
#     n = site
#     ) %>% 
#   mutate(n = as.integer(n)) %>% 
#   mutate(temp = (tmax + tmin / 2)) %>%
#   mutate(date = as.Date(doy, origin = paste0(year, "-01-01")) - 1) %>%
#   dplyr::select(lat, lon, n, year, date, doy, prcp, temp, vp) %>% 
#   right_join(df_smooth, by = c("lat", "lon", "n", "year", "date", "doy"))
# write_rds(df_daymet, str_c(.path$dat_process, "2023-04-25/smooth_daymet.rds"))


# extract annual climate data
df_metrics_daymet <- df_daymet %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>% 
  summarise(
    mat = mean(temp, na.rm = T),
    tap = sum(prcp, na.rm = T),
    mvp = mean(vp, na.rm = T),
  ) %>%
  right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))

write_rds(df_metrics_daymet, str_c(.path$dat_process, "2023-04-25/metrics_amplitude.rds"))
