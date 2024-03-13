labelfunc_x <- function(x) {
  origin <- as.Date("2003-01-01")
  format(origin + x, format = "%b")
}

plot_calendar <- function(df_in) {
  out_gg <- ggplot(data = df_in) +
    geom_tile(aes(x = doy, y = reorder(ylab, order), fill = count_rescale %>% log(10)), alpha = 1) +
    scale_fill_gradient(
      low = "light yellow", high = "dark red", na.value = "white",
      breaks = c(1, 100, 1000, 10000, 1000000) %>% log(10),
      labels = c(1, 100, 1000, 10000, 1000000),
      limits = c(100, 50000) %>% log(10),
      name = expression(atop("Spore concentration (grains m"^-3*")"))) +
    scale_x_continuous(labels = labelfunc_x) +
    ylab("") +
    xlab("") +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.text.y = element_text(color = "black")) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(t = -20, r = 0, b = 0, l = 0))
    
  return(out_gg)
}