p_g <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = df_trend_ain, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8) +
  scale_size_continuous(
    range = c(5, 7),
    breaks = round(seq(min(df_trend_ain$Nyear), max(df_trend_ain$Nyear), length.out = 5)),
    name = "Number of year") +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
    labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))},
    name = "Temporal trend of\nln(annual integral)") +
  geom_point(data = df_trend_ain, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("G") +
  guides(
    size = "none",
    shape = "none") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 0, 0),
        plot.title = element_text(face = "bold"),
        legend.position = "left")
