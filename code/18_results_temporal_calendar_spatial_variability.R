# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_calendar <- df_smooth %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, doy) %>%
  summarise(count_mean = mean(count, na.rm = T)) %>%
  mutate(count_mean = ifelse(is.nan(count_mean), NA, count_mean)) %>% 
  mutate(count_new = ifelse(count_mean < 50000, count_mean, 50000)) %>% 
  mutate(count_new = ifelse(count_mean > 100, count_mean, 100)) %>% 
  left_join(
    df_metrics %>% 
      group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
      summarise(
        tap_mean = mean(tap),
        mat_mean = mean(mat)
      ),
    by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")
  ) %>% 
  mutate(ylab_lon = paste0(city, ", ", state, " (",round(-lon), "° W)")) %>% 
  mutate(ylab_tap = paste0(city, ", ", state, " (", round(tap_mean), " mm)"))

labelfunc_x <- function(x) {
  origin <- as.Date("2003-01-01")
  format(origin + x, format = "%b")
}

p_calendar_a <- ggplot(data = df_calendar %>% filter(n %in% c(10, 9, 22, 13, 6))) +
  geom_tile(aes(x = doy, y = reorder(ylab_lon, lon), fill = count_new %>% log(10)), alpha = 1) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 100, 1000, 10000, 1000000) %>% log(10),
    labels = c(1, 100, 1000, 10000, 1000000),
    limits = c(100, 50000) %>% log(10),
    name = expression(atop("Spore concentration (grains*m"^-3*")"))
  ) +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  ylab("") +
  xlab("") +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0)
  ) +
  labs(title = expression(paste(bold("A")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

p_calendar_b <- ggplot(data = df_calendar %>% filter(n %in% c(5, 7, 48, 56, 40))) +
  geom_tile(aes(x = doy, y = reorder(ylab_tap, tap_mean), fill = count_new %>% log(10)), alpha = 1) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 100, 1000, 10000, 1000000) %>% log(10),
    labels = c(1, 100, 1000, 10000, 1000000),
    limits = c(100, 50000) %>% log(10),
    name = expression(atop("Spore concentration (grains*m"^-3*")"))
  ) +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  ylab("") +
  xlab("") +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0)
  ) +
  labs(title = expression(paste(bold("B")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

legend_grob <- get_legend(p_calendar_a)
p_calendar_r1 <- plot_grid(p_calendar_a + theme(legend.position = 'none'), p_calendar_b + theme(legend.position = 'none'), ncol = 2, align = "v", rel_widths = c(1, 1))
p_calendar <- plot_grid(p_calendar_r1, legend_grob, ncol = 1, rel_heights = c(4, 1))

p_calendar_suppl <- ggplot(data = df_calendar) +
  geom_tile(aes(x = doy, y = reorder(ylab_lon, lon), fill = count_new %>% log(10)), alpha = 1) +
  ylab("") +
  xlab("") +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0)
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 100, 10000, 1000000) %>% log(10),
    labels = c(1, 100, 10000, 1000000),
    limits = c(100, 50000) %>% log(10),
    name = expression(atop("Spore concentration (grains*m"^-3*")"))
  ) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )
