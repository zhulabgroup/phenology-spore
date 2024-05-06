plot_trend_ctnt_pred <- function(df_in, model, metric, pct) {
  beta_value <- fixef(model)[["year_new"]] %>% as.numeric() %>% round(3)
  p_value <- summary(model)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(3)
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
  df_ci <- ggpredict(model, terms = c("year_new", "n"), type = "re") %>%
    as_tibble()
   
  out_gg <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80") +
    geom_point(
      data = df_lme,
      aes(x = year_new, y = Value, col = col),
      alpha = 0.8,
      size = 0.7) +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_random, group = n, col = col),
      alpha = 0.8,
      linewidth = 0.3) +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")) +
    xlab("Year") +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  
  if (p_value < 0.001) {
    out_gg <- out_gg +
      annotate(
        "text",
        x = min(df_lme$year_new), y = max(df_lme$Value),
        label = bquote(atop(italic(beta) == .(beta_value), italic(p) < 0.001)),
        hjust = 0, vjust = 1, col = "black", fontface = "plain")
  } else {
    out_gg <- out_gg +
      annotate(
        "text",
        x = min(df_lme$year_new), y = max(df_lme$Value),
        label = bquote(atop(italic(beta) == .(beta_value), italic(p) == .(p_value))),
        hjust = 0, vjust = 1, col = "black", fontface = "plain")
  }
  
  if (metric %in% c("ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    out_gg <- out_gg + scale_y_continuous(labels = scales::math_format(e^.x))
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
      ylab(expression("Amplitude (spores m"^-3*")")) +
      labs(title = expression(paste("Decreased Ca")))
  }
  
  if (metric == "ln_Cp") {
    out_gg <- out_gg + 
      ylab(expression("Peak concentration (spores m"^-3*")")) +
      labs(title = expression(paste("No change in Cp")))
  }
  
  if (metric == "ln_AIn") {
    out_gg <- out_gg + 
      ylab(expression("Annual integral (spores m"^-3*" days)")) +
      labs(title = expression(paste("No change in AIn")))
  }
  
  if (metric == "ln_ASIn") {
    out_gg <- out_gg + 
      ylab(expression("Allergy season integral (spores m"^-3*" days)")) +
      labs(title = expression(paste("Decreased ASIn")))
  }
  
  return(out_gg)
}
