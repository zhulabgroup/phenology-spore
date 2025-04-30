#' @export
plot_ts <- function(df_ts) {
  p_curves <- df_ts %>%
    filter(n %in% c(10, 9, 13, 22, 6, 5, 44, 7, 38, 52)) %>%
    ggplot() +
    geom_point(
      aes(x = date, y = count),
      col = "gray",
      size = 0.1
    ) +
    geom_line(
      aes(x = date, y = count_fillwhit),
      col = "black"
    ) +
    scale_y_continuous(
      trans = "log10",
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ylab(expression("Spore concentration (spores m"^-3 * ")")) +
    xlab("Date") +
    scale_x_date(breaks = "2 year", date_labels = "%Y") +
    facet_wrap(. ~ interaction(city, state, sep = ", "), ncol = 2) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  return(p_curves)
}
