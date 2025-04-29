#' @export
plot_trend_ctnt_pred <- function(df_analysis) {
  p_trend_ctnt_pred_list <- list()
  for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    # fit lme model
    m_trend <- calc_trend_ctnt(df_in = df_analysis, metric = m_metric, pct = 0.8)
    # acquire col for each station
    df_trend_station <- calc_trend_station_TS(df_in = df_analysis, metric = m_metric, pct = 0.8)
    p_trend_map <- plot_trend_map_TS(df_in = df_trend_station, metric = m_metric)
    df_trend_station_col <- extr_color_map(df_in = df_trend_station, p_map = p_trend_map)
    # plot
    p_trend_ctnt_pred <- plot_trend_ctnt_pred_metric(df_in = df_trend_station_col, model = m_trend, metric = m_metric, pct = 0.8)
    p_trend_ctnt_pred_list[[m_metric]] <- p_trend_ctnt_pred
  }

  title1 <- cowplot::ggdraw() + cowplot::draw_label("Ecology Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
  title2 <- cowplot::ggdraw() + cowplot::draw_label("Public Health Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
  p_trend_ctnt_pred_composite_r1 <- cowplot::plot_grid(
    p_trend_ctnt_pred_list[[1]], p_trend_ctnt_pred_list[[3]], p_trend_ctnt_pred_list[[5]], p_trend_ctnt_pred_list[[7]], p_trend_ctnt_pred_list[[9]],
    nrow = 1,
    labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
    label_fontface = "bold",
    label_size = 12,
    rel_widths = c(1, 1, 1, 1, 1)
  )
  p_trend_ctnt_pred_composite_r2 <- cowplot::plot_grid(
    p_trend_ctnt_pred_list[[2]], p_trend_ctnt_pred_list[[4]], p_trend_ctnt_pred_list[[6]], p_trend_ctnt_pred_list[[8]], p_trend_ctnt_pred_list[[10]],
    nrow = 1,
    labels = c("(f)", "(g)", "(h)", "(i)", "(j)"),
    label_fontface = "bold",
    label_size = 12,
    rel_widths = c(1, 1, 1, 1, 1)
  )
  p_trend_ctnt_pred_composite <- cowplot::plot_grid(
    title1,
    p_trend_ctnt_pred_composite_r1,
    title2,
    p_trend_ctnt_pred_composite_r2,
    nrow = 4,
    rel_heights = c(0.1, 1, 0.1, 1)
  )

  return(p_trend_ctnt_pred_composite)
}

#' @export
plot_trend_ctnt_pred_metric <- function(df_in, model, metric, pct) {
  beta_value <- nlme::fixef(model)[["year_new"]] %>%
    as.numeric() %>%
    round(3)
  p_value <- summary(model)$tTable[["year_new", "p-value"]] %>%
    as.numeric() %>%
    round(3)
  station_slope <- coef(model) %>%
    rename("random_intercept" = "(Intercept)") %>%
    rename(random_slope = year_new)
  station_slope$n <- rownames(station_slope)
  df_lme <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>%
    ungroup() %>%
    mutate(
      lme_fixed = model$fitted[, 1],
      lme_random = model$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    as_tibble() %>%
    left_join(station_slope, by = "n")
  df_ci <- ggeffects::ggpredict(model, terms = c("year_new", "n"), type = "random") %>%
    as_tibble()

  out_gg <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80"
    ) +
    geom_point(
      data = df_lme,
      aes(x = year_new, y = Value, col = col),
      alpha = 0.8,
      size = 0.7
    ) +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_random, group = n, col = col),
      alpha = 0.8,
      linewidth = 0.3
    ) +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")
    ) +
    xlab("Year") +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  if (p_value < 0.001) {
    out_gg <- out_gg +
      annotate(
        "text",
        x = min(df_lme$year_new), y = max(df_lme$Value),
        label = bquote(atop(italic(beta) == .(beta_value), italic(p) < 0.001)),
        hjust = 0, vjust = 1, col = "black", fontface = "plain"
      )
  } else {
    out_gg <- out_gg +
      annotate(
        "text",
        x = min(df_lme$year_new), y = max(df_lme$Value),
        label = bquote(atop(italic(beta) == .(beta_value), italic(p) == .(p_value))),
        hjust = 0, vjust = 1, col = "black", fontface = "plain"
      )
  }

  if (metric %in% c("ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    out_gg <- out_gg + scale_y_continuous(
      labels = scales::math_format(e^.x)
    )
  }

  if (metric == "SOS") {
    out_gg <- out_gg +
      ylab("Start of season (day of spore year)") +
      labs(title = expression(paste("Advanced SOS")))
  }

  if (metric == "SAS") {
    out_gg <- out_gg +
      ylab("Start of allergy season (day of spore year)") +
      labs(title = expression(paste("Advanced SAS")))
  }

  if (metric == "EOS") {
    out_gg <- out_gg +
      ylab("End of season (day of spore year)") +
      labs(title = expression(paste("No change in EOS")))
  }

  if (metric == "EAS") {
    out_gg <- out_gg +
      ylab("End of allergy season (day of spore year)") +
      labs(title = expression(paste("Advanced EAS")))
  }

  if (metric == "LOS") {
    out_gg <- out_gg +
      ylab("Length of season (days)") +
      labs(title = expression(paste("No change in LOS")))
  }

  if (metric == "LAS") {
    out_gg <- out_gg +
      ylab("Length of allergy season (days)") +
      labs(title = expression(paste("No change in LAS")))
  }

  if (metric == "ln_Ca") {
    out_gg <- out_gg +
      ylab(expression("Amplitude (spores m"^-3 * ")")) +
      labs(title = expression(paste("Decreased Ca")))
  }

  if (metric == "ln_Cp") {
    out_gg <- out_gg +
      ylab(expression("Peak concentration (spores m"^-3 * ")")) +
      labs(title = expression(paste("No change in Cp")))
  }

  if (metric == "ln_AIn") {
    out_gg <- out_gg +
      ylab(expression("Annual integral (spores m"^-3 * " days)")) +
      labs(title = expression(paste("No change in AIn")))
  }

  if (metric == "ln_ASIn") {
    out_gg <- out_gg +
      ylab(expression("Allergy season integral (spores m"^-3 * " days)")) +
      labs(title = expression(paste("Decreased ASIn")))
  }

  return(out_gg)
}

extr_color_map <- function(df_in, p_map) {
  df <- ggplot_build(p_map)$data[[3]][c("colour", "x", "y")] %>%
    distinct(x, .keep_all = T) %>%
    rename(
      col = colour,
      lon = x,
      lat = y
    ) %>%
    right_join(
      df_in %>% dplyr::select(-col),
      by = c("lon", "lat")
    )

  return(df)
}
