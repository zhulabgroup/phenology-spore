# read the offset of each station from turbo

df_offset <- read_rds(str_c(.path$dat_process, "fill_smooth_offset.rds")) %>%
  group_by(lat, lon, station, state, country, id, lambda, offset) %>%
  summarise(
    lat = head(lat, 1),
    lon = head(lon, 1),
    station = head(station, 1),
    state = head(state, 1),
    country = head(country, 1),
    id = head(id, 1),
    lambda = head(lambda, 1),
    offset = head(offset, 1),
  ) %>%
  ungroup() %>%
  filter(country == "US") %>%
  filter(!(state %in% c("PR", "AK", "HI")))
