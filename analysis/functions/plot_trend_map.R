plot_trend_map <- function(df_in, metric) {
  p_map <- ggplot() +
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
    geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
    # coord_map("conic", lat0 = 30) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    scale_y_continuous(
      breaks = c(25, 35, 45),
      labels = function(x) paste0(x, "\u00B0 N")) +
    scale_x_continuous(
      breaks = c(-120, -100, -80),
      labels = function(x) ifelse(x < 0, paste0(abs(x), "\u00B0 W"), paste0(x, "\u00B0 E"))) +
    labs(x = "Longitude", y = "Latitude") +
    geom_point(
      data = df_in, 
      aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
      alpha = 0.8) +
    geom_point(
      data = df_in,
      aes(x = lon, y = lat, size = Nyear),
      shape = 1,
      fill = "transparent",
      col = "grey40",
      alpha = 0.5) +
    scale_size_continuous(
      range = c(5, 7),
      breaks = round(seq(min(df_in$Nyear), max(df_in$Nyear), length.out = 5)),
      name = "Number\nof years") +
    # geom_point(
    #   data = df_in,
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
  theme(
    legend.position = c(0.01, 0.01),
    legend.box = "horizontal",
    legend.box.just = "bottom",
    legend.justification = c(0, 0),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.background = element_blank(),
    legend.key = element_blank())
    
  
  #add color palette
  if (metric == "SOS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of SOS\n(days per year)"))
  }
  if (metric == "SAS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of SAS\n(days per year)"))
  }
  if (metric == "EOS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of EOS\n(days per year)"))
  }
  if (metric == "EAS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "red", mid= "white", high = "blue", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of EAS\n(days per year)"))
  }
  if (metric == "LOS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of LOS\n(days per year)"))
  }
  if (metric == "LAS") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of LAS\n(days per year)"))
  }
  if (metric == "ln_Ca") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of ln(Ca)"))
  }
  if (metric == "ln_Cp") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of ln(Cp)"))
  }
  if (metric == "ln_AIn") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of ln(AIn)"))
  }
  if (metric == "ln_ASIn") {
    p_map <- p_map +
      scale_color_gradient2(
        low = "blue", mid= "white", high = "red", midpoint = 0,
        labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
      labs(color = expression("Trend of ln(ASIn)"))
  }
  
  return(p_map)
}
