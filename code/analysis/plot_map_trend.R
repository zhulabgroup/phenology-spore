plot_map_trend <- function(df_in, metric) {
  p_map <- ggplot() +
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
    geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
    coord_map("conic", lat0 = 30) +
    theme_void() +
    geom_point(
      data = df_lm_trend, 
      aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
      alpha = 0.8) +
    scale_size_continuous(
      range = c(5, 7),
      breaks = round(seq(min(df_lm_trend$Nyear), max(df_lm_trend$Nyear), length.out = 5)),
      name = "Number of year") +
    geom_point(
      data = df_lm_trend, 
      aes(x = lon, y = lat, size = Nyear),
      shape = 1,
      col = "grey40",
      alpha = 0.5) +
    # geom_point(
    #   data = df_lm_trend,
    #   aes(
    #     x = lon, y = lat,
    #     shape = ifelse(p_value >= 0.05, ">= 0.05", "< 0.05"),
    #     size = Nyear)) +
    # scale_shape_manual(
    #   values = c(10, 1),
    #   guide = guide_legend(
    #     title = expression(italic("p-value")),
    #     override.aes = list(size = c(4, 4)))) +
  # guides(size = "none", shape = "none") +
  theme(legend.position = "left")
  
  #add color palette
  if (metric == "SOS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        # breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))},
        name = expression("Temporal trend of\nSOS (days year"^-1*")"))
  }
  if (metric == "SAS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        # breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))},
        name = expression("Temporal trend of\nSOS (days year"^-1*")"))
  }
  
  return(p_map)
}
