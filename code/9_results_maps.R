df_metrics <- read_rds(str_c(.path$dat_process, "metrics_with_climate.rds"))
df_smooth <- read_rds(str_c(.path$dat_process, "fill_smooth_to2021.rds"))

# map: peak
data_peak <- df_metrics %>%
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
  dplyr::select(location, id, lat, lon, year, peak) %>%
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup() %>%
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(log(peak + 1) ~ year, .)
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
  rename("slope" = "year")
plot_map_peak <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_peak %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_peak %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of peak concentration (log transformation)")
ggsave(
  plot = plot_map_peak,
  filename = "~/spore_phenology/output/figures/plot_map_peak.pdf",
  width = 15,
  height = 10
)

# map: peak_doy
data_peak <- df_metrics %>%
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
  dplyr::select(location, id, lat, lon, year, peak_doy) %>%
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
  rename("slope" = "year")
# plot
plot_map_peak_doy <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_peak, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_peak %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_peak %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of peak doy")
ggsave(
  plot = plot_map_peak_doy,
  filename = "~/spore_phenology/output/figures/plot_map_peak_doy.pdf",
  width = 15,
  height = 10
)

# map: integral
data_integral <- df_smooth %>%
  group_by(location, id, year) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  drop_na(count_smooth) %>%
  filter(count_smooth > 5) %>% 
  summarise(observ_percent = n() / 214) %>%
  full_join(df_metrics, by = c("location" = "location", "id" = "id", "year" = "year")) %>%
  drop_na(integral) %>%
  filter(observ_percent >= 0.5) %>%
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
  rename("slope" = "year")
#
plot_map_integral <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_integral, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_integral %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_integral %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of integral in Apr-Oct (log transformation)")
ggsave(
  plot = plot_map_integral,
  filename = "~/spore_phenology/output/figures/plot_map_integral.pdf",
  width = 15,
  height = 10
)

# map: sos
data_season <- df_metrics %>%
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
  dplyr::select(location, id, lat, lon, year, sos, eos, los) %>% 
  group_by(lat, lon, location, id) %>%
  mutate(Nyear = max(year) - min(year) + 1) %>%
  ungroup()
data_sos <- data_season %>% 
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(sos ~ year, .)
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
  rename("slope" = "year")
#
plot_map_sos <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_sos, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_sos %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_sos %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of sos")
ggsave(
  plot = plot_map_sos,
  filename = "~/spore_phenology/output/figures/plot_map_sos.pdf",
  width = 15,
  height = 10
)

# map: eos
data_eos <- data_season %>% 
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(eos ~ year, .)
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
  rename("slope" = "year")
#
plot_map_eos <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_eos, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark red", high = "dark blue") +
  geom_point(data = data_eos %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_eos %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of eos")
ggsave(
  plot = plot_map_eos,
  filename = "~/spore_phenology/output/figures/plot_map_eos.pdf",
  width = 15,
  height = 10
)

# map: los
data_los <- data_season %>% 
  group_by(lat, lon, location, id, Nyear) %>%
  do({
    result <- lm(los ~ year, .)
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
  rename("slope" = "year")
#
plot_map_los <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_los, aes(x = lon, y = lat, size = Nyear, color = slope), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "dark blue", high = "dark red") +
  geom_point(data = data_los %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_los %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape = 10, color = "black") +
  ggtitle("Temporal trends of eos")
ggsave(
  plot = plot_map_los,
  filename = "~/spore_phenology/output/figures/plot_map_los.pdf",
  width = 15,
  height = 10
)
