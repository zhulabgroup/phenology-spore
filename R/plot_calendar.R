#' @export
plot_calendar <- function(df_ts) {
  df_calendar_a <- calc_calendar_samples(df_ts, df_metrics, df_daymet_annual, y_label = "ecoregion")
  p_calendar_a <- plot_calendar_samples(df_calendar_a, y_label = "ecoregion")

  df_calendar_b <- calc_calendar_samples(df_ts, df_metrics, df_daymet_annual, y_label = "mm")
  p_calendar_b <- plot_calendar_samples(df_calendar_b, y_label = "mm")

  p_calendar <- (p_calendar_a + p_calendar_b) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom") +
      plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(size = 12, face = "bold")) &
    plot_annotation(tag_prefix = "(", tag_suffix = ")")

  return(p_calendar)
}


labelfunc_x <- function(x) {
  origin <- as.Date("2003-01-01")
  format(origin + x, format = "%b")
}

#' @export
plot_calendar_samples <- function(df_in, y_label) {
  out_gg <- ggplot(data = df_in) +
    geom_tile(aes(x = doy, y = reorder(ylab, order), fill = count_rescale %>% log(10)), alpha = 1) +
    scale_fill_gradient(
      low = "light yellow", high = "dark red", na.value = "white",
      breaks = c(1, 100, 1000, 10000, 1000000) %>% log(10),
      labels = c(1, 100, 1000, 10000, 1000000),
      limits = c(100, 50000) %>% log(10),
      name = expression(atop("Spore concentration (spores m"^-3 * ")"))
    ) +
    scale_x_continuous(
      breaks = c(1, 91, 182, 274),
      labels = labelfunc_x
    ) +
    ylab("") +
    xlab("") +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.text.y = element_text(color = "black")
    ) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(t = -20, r = 0, b = 0, l = 0)
    )

  if (y_label == "lon") {
    out_gg <- out_gg +
      geom_tile(data = df_in %>% filter(doy == offset), aes(x = doy, y = reorder(ylab, order)), fill = "black")
  }

  return(out_gg)
}

calc_calendar_samples <- function(df_in, df_metrics, df_daymet_annual, y_label) {
  df <- df_in %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, doy) %>%
    summarise(count_mean = mean(count, na.rm = T)) %>%
    mutate(count_mean = ifelse(is.nan(count_mean), NA, count_mean)) %>%
    mutate(count_rescale = ifelse(count_mean < 50000, count_mean, 50000)) %>%
    mutate(count_rescale = ifelse(count_rescale > 100, count_rescale, 100)) %>%
    subset(doy != 366) %>%
    left_join(
      left_join(df_metrics, df_daymet_annual, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new")) %>%
        group_by(lat, lon, station, city, state, country, id, n, offset) %>%
        summarise(
          tap_mean = mean(tap),
          mat_mean = mean(mat)
        ),
      by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")
    )

  if (y_label == "lon") {
    df_out <- df %>%
      mutate(ylab = paste0(city, ", ", state, " (", round(-lon), "° W)")) %>%
      mutate(order = lon)
  } else if (y_label == "ecoregion") {
    df_out <- df %>%
      filter(n %in% c(10, 9, 13, 22, 6)) %>%
      mutate(eco = "Mediterranean California") %>%
      mutate(eco = ifelse(state == "OH", "Eastern Temperate Forests", eco)) %>%
      mutate(eco = ifelse(state == "MO", "Eastern Temperate Forests", eco)) %>%
      mutate(eco = ifelse(state == "OK", "Great Plains", eco)) %>%
      mutate(ylab = paste0(city, ", ", state, "\n(", eco, ")")) %>%
      mutate(order = lon)
  } else if (y_label == "mm") {
    df_out <- df %>%
      filter(n %in% c(5, 44, 7, 38, 52)) %>%
      mutate(ylab = paste0(city, ", ", state, "\n(", round(tap_mean), " mm)")) %>%
      mutate(order = tap_mean)
  }

  return(df_out)
}
