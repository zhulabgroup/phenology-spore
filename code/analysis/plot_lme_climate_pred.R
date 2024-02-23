plot_lme_climate_pred <- function(m_lme, df_in, metric, x_lab, pct) {
  beta_value <- fixef(m_lme)[["climate"]] %>% as.numeric() %>% round(5)
  p_value <- summary(m_lme)$tTable[["climate", "p-value"]] %>% as.numeric() %>% round(5)
  # extract station-specific slope and intercept
  df_random <- coef(m_lme) %>% 
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
      lme_fixed = m_lme$fitted[, 1],
      lme_random = m_lme$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    left_join(df_random, by = "n") %>% 
    as_tibble()
  if (x_lab == "MAT") {
    df_lme <- df_lme %>% 
      rename(climate = mat)
  } else {
    df_lme <- df_lme %>% 
      rename(climate = tap)
  }
  # extract CI
  df_ci <- ggpredict(m_lme, terms = c("climate", "n"), type = "re") %>%
    as_tibble()
  # plot
  p_lme <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80") +
    geom_point(
      data = df_lme,
      aes(x = climate, y = Value, col = n_slope),
      alpha = 0.8,
      size = 0.5) +
    geom_line(
      data = df_lme,
      aes(x = climate, y = lme_random, group = n, col = n_slope),
      alpha = 0.8,
      linewidth = 0.3) +
    # scale_color_identity() +
    scale_color_gradient2(
      low = "red", mid= "white", high = "blue", midpoint = 0,
      # breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
      labels = function(x) {
        ifelse(x == 0, "0", paste0(x, "\u00B3"))}) +
    geom_line(
      data = df_lme,
      aes(x = climate, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")) +
    theme_classic() +
    xlab(x_lab) +
    ylab(metric) +
    # ylab(paste0("Anomaly of ", y_lab)) +
    # theme(
    #   plot.title.position = "plot",
    #   plot.margin = margin(10, 10, 10, 10)) +
    theme(
      # legend.position = "none",
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.y = element_text(color = "black"))
  
  return(p_lme)
}