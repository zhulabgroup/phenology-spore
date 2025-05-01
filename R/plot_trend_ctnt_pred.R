#' @export
plot_trend_ctnt_pred <- function(df_analysis, ls_x_var = "year_new", pct = 0.8, annotate = T, y_label_short = F) {
  p_trend_ctnt_pred_list <- list()
  for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    for (x_var in ls_x_var) {
      # fit lme model
      m_trend <- calc_trend_ctnt(df_in = df_analysis, metric = m_metric, x_var = x_var, pct = pct)
      # acquire col for each station
      df_trend_station <- calc_trend_station_TS(df_in = df_analysis, ls_metric = m_metric, pct = pct)
      p_trend_map <- plot_trend_map_TS(df_in = df_trend_station, ls_metric = m_metric)
      df_trend_station_col <- extr_color_map(df_in = df_trend_station, p_map = p_trend_map)
      # plot
      p_trend_ctnt_pred <- plot_trend_ctnt_pred_metric(df_in = df_trend_station_col, model = m_trend, metric = m_metric, x_var = x_var, pct = pct, annotate = annotate, y_label_short = y_label_short)
      p_trend_ctnt_pred_list[[str_c(m_metric, x_var, sep = "_")]] <- p_trend_ctnt_pred
    }
  }

  if (length(ls_x_var) == 1) {
    p_trend_ctnt_pred_composite_list <- p_trend_ctnt_pred_list
  } else {
    p_trend_ctnt_pred_composite_list <- list()
    for (am in c(1:10)) {
      p_trend_ctnt_pred_composite_list[[am]] <- cowplot::plot_grid(
        p_trend_ctnt_pred_list[[length(ls_x_var) * am - 1]], p_trend_ctnt_pred_list[[length(ls_x_var) * am]],
        ncol = 1
      )
    }
  }

  title1 <- cowplot::ggdraw() + cowplot::draw_label("Ecology Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
  title2 <- cowplot::ggdraw() + cowplot::draw_label("Public Health Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
  p_trend_ctnt_pred_composite_r1 <- cowplot::plot_grid(
    p_trend_ctnt_pred_composite_list[[1]], p_trend_ctnt_pred_composite_list[[3]], p_trend_ctnt_pred_composite_list[[5]], p_trend_ctnt_pred_composite_list[[7]], p_trend_ctnt_pred_composite_list[[9]],
    nrow = 1,
    labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
    label_fontface = "bold",
    label_size = 12,
    rel_widths = c(1, 1, 1, 1, 1)
  )
  p_trend_ctnt_pred_composite_r2 <- cowplot::plot_grid(
    p_trend_ctnt_pred_composite_list[[2]], p_trend_ctnt_pred_composite_list[[4]], p_trend_ctnt_pred_composite_list[[6]], p_trend_ctnt_pred_composite_list[[8]], p_trend_ctnt_pred_composite_list[[10]],
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
plot_trend_ctnt_pred_metric <- function(df_in, model, metric, x_var = "year_new", pct = 0.8, annotate = T, y_label_short = F) {
  beta_value <- nlme::fixef(model)[["x"]] %>%
    as.numeric() %>%
    round(3)
  p_value <- summary(model)$tTable[["x", "p-value"]] %>%
    as.numeric() %>%
    round(3)
  station_slope <- coef(model) %>%
    rename("random_intercept" = "(Intercept)") %>%
    rename(random_slope = "x") %>%
    mutate(n = rownames(.))
  df_lme <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(
      lme_fixed = model$fitted[, 1],
      lme_random = model$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    as_tibble() %>%
    left_join(station_slope, by = "n")
  df_ci <- ggeffects::ggpredict(model, terms = c("x", "n"), type = "random") %>%
    as_tibble()

  out_gg <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80"
    ) +
    geom_point(
      data = df_lme,
      aes(x = !!sym(x_var), y = Value, col = col),
      alpha = 0.8,
      size = 0.7
    ) +
    geom_line(
      data = df_lme,
      aes(x = !!sym(x_var), y = lme_random, group = n, col = col),
      alpha = 0.8,
      linewidth = 0.3
    ) +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = !!sym(x_var), y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")
    ) +
    xlab(
      case_when(
        x_var == "year_new" ~ "Year",
        x_var == "mat" ~ "MAT (°C)",
        x_var == "tap" ~ "TAP (mm)"
      )
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  if (annotate) {
    if (p_value < 0.001) {
      out_gg <- out_gg +
        annotate(
          "text",
          x = df_lme %>% pull(!!sym(x_var)) %>% min(), y = df_lme %>% pull(Value) %>% max(),
          label = bquote(atop(italic(beta) == .(beta_value), italic(p) < 0.001)),
          hjust = 0, vjust = 1, col = "black", fontface = "plain"
        )
    } else {
      out_gg <- out_gg +
        annotate(
          "text",
          x = df_lme %>% pull(!!sym(x_var)) %>% min(), y = df_lme %>% pull(Value) %>% max(),
          label = bquote(atop(italic(beta) == .(beta_value), italic(p) == .(p_value))),
          hjust = 0, vjust = 1, col = "black", fontface = "plain"
        )
    }
  }

  if (metric %in% c("ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    out_gg <- out_gg + scale_y_continuous(
      labels = scales::math_format(e^.x)
    )
  }

  out_gg <- out_gg +
    ylab(y_label(metric, short = y_label_short))

  if (annotate) {
    out_gg <- out_gg +
      labs(title = title_label(beta_value, p_value, metric))
  }
  return(out_gg)
}

y_label <- function(metric, short = F) {
  labels_expr <- list(
    SOS     = list(short = expression("SOS (DOY)"), long = expression("Start of season (day of spore year)")),
    SAS     = list(short = expression("SAS (DOY)"), long = expression("Start of allergy season (day of spore year)")),
    EOS     = list(short = expression("EOS (DOY)"), long = expression("End of season (day of spore year)")),
    EAS     = list(short = expression("EAS (DOY)"), long = expression("End of allergy season (day of spore year)")),
    LOS     = list(short = expression("LOS (days)"), long = expression("Length of season (days)")),
    LAS     = list(short = expression("LAS (days)"), long = expression("Length of allergy season (days)")),
    ln_Ca   = list(short = expression("Ca (spores m"^-3 * ")"), long = expression("Amplitude (spores m"^-3 * ")")),
    ln_Cp   = list(short = expression("Cp (spores m"^-3 * ")"), long = expression("Peak concentration (spores m"^-3 * ")")),
    ln_AIn  = list(short = expression("AIn (spores m"^-3 * " days)"), long = expression("Annual integral (spores m"^-3 * " days)")),
    ln_ASIn = list(short = expression("ASIn (spores m"^-3 * " days)"), long = expression("Allergy season integral (spores m"^-3 * " days)"))
  )

  label <- if (short) labels_expr[[metric]]$short else labels_expr[[metric]]$long

  return(label)
}

title_label <- function(beta_value, p_value, metric) {
  if (p_value > 0.05) {
    label1 <- "No change in "
  } else {
    if (beta_value > 0) {
      if (metric %in% c("SOS", "SAS", "EOS", "EAS")) {
        label1 <- "Delayed "
      } else {
        label1 <- "Increased "
      }
    } else {
      if (metric %in% c("SOS", "SAS", "EOS", "EAS")) {
        label1 <- "Advanced "
      } else {
        label1 <- "Decreased "
      }
    }
  }

  label2 <- str_remove(metric, "ln_")
  label <- str_c(label1, label2)

  return(label)
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
