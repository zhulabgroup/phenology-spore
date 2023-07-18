# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))


df_calendar <- df_smooth %>%
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, doy) %>%
  summarise(count_mean = mean(count, na.rm = T)) %>%
  mutate() %>% 
  ungroup() %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>%
  ungroup() %>% 
  mutate(city = factor(city)) %>% 
  mutate(state = factor(state)) %>% 
  mutate(n = factor(n)) %>% 
  arrange(lon) %>% 
  mutate(order = interaction(city, state, n)) %>% 
  mutate(count_re = ifelse(count_mean < 6500, count_mean, 6500))

labelfunc_x <- function(x) {
  origin <- as.Date("2003-01-01")
  format(origin + x, format = "%b")
}

p_calendar_a <- ggplot(data = df_calendar %>% filter(n %in% c(10, 9, 22, 29, 6))) +
  geom_tile(aes(x = doy, y = reorder(interaction(city, state, n), lon), fill = count_re), alpha = 1) +
  ylab("") +
  xlab("") +
  scale_y_discrete(labels = function(x) {
    parts <- strsplit(as.character(x), "\\.")
    city <- sapply(parts, "[", 1)
    state <- sapply(parts, "[", 2)
    n <- sapply(parts, "[", 3)
    paste(city, state, n, sep = ", ")
  }) +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  # scale_x_continuous(labels = labelfunc_x) +
  theme(
    axis.line.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # theme(strip.text.y= element_text(angle = 0))+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 1000, 3000, 5000),
    labels = c(1, 1000, 3000, 5000),
    limits = c(1, 6500),
    name = "Spore concentration\n(grains / m^3)"
  ) +
  theme(
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("A")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

p_calendar_b <- ggplot(data = df_calendar %>% filter(n %in% c(5, 7, 48, 56, 40))) +
  geom_tile(aes(x = doy, y = reorder(interaction(city, state, n), lat), fill = count_re), alpha = 1) +
  ylab("") +
  scale_y_discrete(labels = function(x) {
    parts <- strsplit(as.character(x), "\\.")
    city <- sapply(parts, "[", 1)
    state <- sapply(parts, "[", 2)
    n <- sapply(parts, "[", 3)
    paste(city, state, n, sep = ", ")
  }) +
  xlab("") +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  # scale_x_continuous(labels = labelfunc_x) +
  theme(
    axis.line.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # theme(strip.text.y= element_text(angle = 0))+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 3000, 5000),
    labels = c(1, 3000, 5000),
    limits = c(1, 6500),
    name = "Spore concentration\n(grains / m^3)"
  ) +
  theme(
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("B")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

p_calendar <- plot_grid(p_calendar_a, p_calendar_b, ncol = 2)

p_calendar_suppl <- ggplot(data = df_calendar) +
  geom_tile(aes(x = doy, y = reorder(interaction(city, state, n), lon), fill = count_re), alpha = 1) +
  ylab("") +
  xlab("") +
  scale_y_discrete(labels = function(x) {
    parts <- strsplit(as.character(x), "\\.")
    city <- sapply(parts, "[", 1)
    state <- sapply(parts, "[", 2)
    n <- sapply(parts, "[", 3)
    paste(city, state, n, sep = ", ")
  }) +
  scale_x_continuous(labels = labelfunc_x) +
  theme_classic() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  # scale_x_continuous(labels = labelfunc_x) +
  theme(
    axis.line.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # theme(strip.text.y= element_text(angle = 0))+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 3000, 5000),
    labels = c(1, 3000, 5000),
    limits = c(1, 6500),
    name = "Spore concentration\n(grains / m^3)"
  ) +
  theme(
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0),
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )
