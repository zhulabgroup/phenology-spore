df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_calendar <- df_smooth %>%
  group_by(lat, lon, station, city, state, country, id, n, offset, doy) %>%
  summarise(count_mean = mean(count, na.rm = T)) %>%
  mutate() %>% 
  ungroup() %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>%
  mutate(count_st = (count_mean - min(count_mean, na.rm = T)) / (max(count_mean, na.rm = T) - min(count_mean, na.rm = T))) %>%
  ungroup() %>% 
  mutate(city = factor(city)) %>% 
  mutate(state = factor(state)) %>% 
  mutate(n = factor(n)) %>% 
  arrange(lon) %>% 
  mutate(order = interaction(city, state, n))

p_calendar <- ggplot(data = df_calendar %>% filter(state != "PR")) +
  geom_tile(aes(x = doy, y = reorder(interaction(city, state, n), lon), fill = count_mean), alpha = 1) +
  ylab("") +
  xlab("") +
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
    legend.key.width = unit(2, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white" # ,
    # breaks=(c(0,1, 100,  10000, 1000000)+1) %>% log(10),
    # labels=c(0,1, 100,  10000, 1000000),
    # name=expression(Spore~concentration~(grains / m^3))
  ) +
  guides(fill = "none")

pdf(
  "output/figures/p_calendar_raw.pdf",
  width = 8 * .618,
  height = 8
)
p_calendar
dev.off()
