parameters_df <- read_rds("~/spore_phenology/data/parameters.rds")
data_smooth_df <- read_rds("~/spore_phenology/data/data_smooth.rds")

# map: peak_con
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
  dplyr::select(location, id, lat, lon, year, peak_con, peak_date) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(log(peak_con + 1) ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")

# map: peak_date
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
  dplyr::select(location, id, lat, lon, year, peak_con, peak_date) %>%
  mutate(peak_doy = format(peak_date, "%j")) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(peak_doy ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")

# map: integral
data_integral <- parameters_df %>%
  dplyr::select(location, id, lat, lon, year, integral) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(log(integral + 1) ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_integral, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_integral, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")

# map: start_date
data_season <- parameters_df %>%
  filter((id == 21 & year %in% c(2014, 2015, 2016, 2017)) |
    (id == 36 & year != 2018) |
    (id == 32) |
    (id == 35 & year != 2013 & year != 2019) |
    (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016)) |
    (id == 49) |
    (id == 37)) %>%
  dplyr::select(location, id, lat, lon, year, start_date, end_date) %>%
  mutate(start_doy = format(start_date, "%j") %>% as.integer()) %>%
  mutate(end_doy = format(end_date, "%j") %>% as.integer()) %>%
  mutate(duration = end_doy - start_doy + 1) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(start_doy ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")

# map: end_date
data_season <- parameters_df %>%
  filter((id == 21 & year %in% c(2014, 2015, 2016, 2017)) |
    (id == 36 & year != 2018) |
    (id == 32) |
    (id == 35 & year != 2013 & year != 2019) |
    (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016)) |
    (id == 49) |
    (id == 37)) %>%
  dplyr::select(location, id, lat, lon, year, start_date, end_date) %>%
  mutate(start_doy = format(start_date, "%j") %>% as.integer()) %>%
  mutate(end_doy = format(end_date, "%j") %>% as.integer()) %>%
  mutate(duration = end_doy - start_doy + 1) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(end_doy ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")

# map: duration
data_season <- parameters_df %>%
  filter((id == 21 & year %in% c(2014, 2015, 2016, 2017)) |
    (id == 36 & year != 2018) |
    (id == 32) |
    (id == 35 & year != 2013 & year != 2019) |
    (id == 50 & year %in% c(2012, 2013, 2014, 2015, 2016)) |
    (id == 49) |
    (id == 37)) %>%
  dplyr::select(location, id, lat, lon, year, start_date, end_date) %>%
  mutate(start_doy = format(start_date, "%j") %>% as.integer()) %>%
  mutate(end_doy = format(end_date, "%j") %>% as.integer()) %>%
  mutate(duration = end_doy - start_doy + 1) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(duration ~ year, .)
    data_frame(
      r_squared =
        result %>%
          summary() %>%
          use_series(adj.r.squared),
      p_value =
        result %>%
          anova() %>%
          use_series(`Pr(>F)`) %>%
          extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "year")
#
ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_season, aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black")
