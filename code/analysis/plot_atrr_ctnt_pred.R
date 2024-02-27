plot_atrr_ctnt_pred <- function(model, df_in, metric, cli_vrb, pct) {
  beta_value <- fixef(model)[["climate"]] %>% as.numeric() %>% round(5)
  p_value <- summary(model)$tTable[["climate", "p-value"]] %>% as.numeric() %>% round(5)
  # extract station-specific slope and intercept
  df_random <- coef(model) %>% 
    rename("n_intercept" = "(Intercept)") %>% 
    rename(n_slope = climate)
  df_random$n <- rownames(df_random)
  df_lme <- df_in %>% 
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup() %>% 
    mutate( # extract predicted value
      lme_fixed = model$fitted[, 1],
      lme_random = model$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    left_join(df_random, by = "n") %>% 
    as_tibble()
  if (cli_vrb == "MAT") {
    df_lme <- df_lme %>% 
      rename(climate = mat)
  } else {
    df_lme <- df_lme %>% 
      rename(climate = tap)
  }
  # extract CI
  df_ci <- ggpredict(model, terms = c("climate", "n"), type = "re") %>%
    as_tibble()
  # plot
  out_gg <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80") +
    geom_point(
      data = df_lme,
      aes(x = climate, y = Value, col = col),
      alpha = 0.8,
      size = 0.5) +
    geom_line(
      data = df_lme,
      aes(x = climate, y = lme_random, group = n, col = col),
      alpha = 0.8,
      linewidth = 0.3) +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = climate, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      text = element_text(size = 10))
  
  if (cli_vrb == "MAT") {
    out_gg <- out_gg +
      xlab("MAT (°C)")
  } else {
    out_gg <- out_gg +
      xlab("TAP (mm)")
  }
  
  if (metric %in% c("ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    out_gg <- out_gg + scale_y_continuous(labels = scales::math_format(e^.x))
  }
  
  if (metric == "SOS") {
    out_gg <- out_gg + 
      ylab("SOS (day of spore year)")
  }
  
  if (metric == "SAS") {
    out_gg <- out_gg + 
      ylab("SAS (day of spore year)")
  }
  
  if (metric == "EOS") {
    out_gg <- out_gg + 
      ylab("EOS (day of spore year)")
  }
  
  if (metric == "EAS") {
    out_gg <- out_gg + 
      ylab("EAS (day of spore year)")
  }
  
  if (metric == "LOS") {
    out_gg <- out_gg + 
      ylab("LOS (days)")
  }
  
  if (metric == "LAS") {
    out_gg <- out_gg + 
      ylab("LAS (days)")
  }
  
  if (metric == "ln_Ca") {
    out_gg <- out_gg + 
      ylab(expression("Ca (grains m"^-3*")"))
  }
  
  if (metric == "ln_Cp") {
    out_gg <- out_gg + 
      ylab(expression("Cp (grains m"^-3*")"))
  }
  
  if (metric == "ln_AIn") {
    out_gg <- out_gg + 
      ylab(expression("AIn (grains m"^-3*" days)"))
  }
  
  if (metric == "ln_ASIn") {
    out_gg <- out_gg + 
      ylab(expression("ASIn (grains m"^-3*" days)"))
  }
  
  return(out_gg)
}