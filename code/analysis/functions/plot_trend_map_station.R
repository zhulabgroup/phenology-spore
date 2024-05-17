plot_trend_map_station <- function(p_map, df_in) {
  out_gg <- p_map +
    geom_point(
      data = df_in,
      aes(x = lon, y = lat),
      shape = 0,
      size = 10,
      color = "dark green",
      fill = "transparent") +
    ggrepel::geom_label_repel(
      data = df_in,
      aes(x = lon, y = lat, label = paste0(city, ", ", state)),
      col = "dark green",
      box.padding = 1,
      point.padding = 1,
      nudge_y = -0.2,
      nudge_x = 0.2)
  
  return(out_gg)
}