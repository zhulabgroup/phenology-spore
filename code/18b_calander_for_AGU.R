p_calendar_a <- ggplot(data_a) +
  geom_tile(aes(x = doy, y = reorder(ylab_eco, lon), fill = count_new %>% log(10)), alpha = 1) +
  scale_fill_gradient(
    low = "light yellow", high = "dark red", na.value = "white",
    breaks = c(1, 100, 1000, 10000, 1000000) %>% log(10),
    labels = c(1, 100, 1000, 10000, 1000000),
    limits = c(100, 50000) %>% log(10),
    name = expression(atop("Spore concentration (grains m"^-3*")"))
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
    plot.title = element_text(size = 30),
    
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(t = -20, r = 0, b = 0, l = 0)
  ) +
  labs(title = expression(paste("A"))) +
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
    name = expression(atop("Spore concentration (grains m"^-3*")"))
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
