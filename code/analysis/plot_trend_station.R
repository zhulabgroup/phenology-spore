plot_trend_station <- function(df_in, station_n, metric) {
  df <- df_in %>% 
    filter(n == station_n)
  
  city_lab <- head(df$city, 1)
  
  plot_col <- head(df$col, 1)
  
  if (metric == "SAS") {
    y_lab = "SAS (day of spore year)"
  }
  
  out_gg <- ggplot(data = df) +
    geom_line(aes(x = year_new, y = Value)) +
    scale_x_continuous(
      limits = c(2002, 2022)) +
    labs(
      x = NULL,
      y = y_lab,
      title = city_lab) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = plot_col),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  
  return(out_gg)
}