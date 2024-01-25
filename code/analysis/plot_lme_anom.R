plot_lme_anom <- function(df_in, m, x_lab, y_lab, lmd) {
  beta <- fixef(m)[["climate_anom"]] %>% as.numeric() %>% round(5)
  p <- summary(m)$tTable[["climate_anom", "p-value"]] %>% as.numeric() %>% round(5)
  
  df_lme <- df_in %>% 
    mutate(
      lme_fixed = m$fitted[, 1],
      lme_random = m$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    as_tibble()
  
  df_ci <- ggpredict(m, terms = c("climate_anom", "n"), type = "re") %>%
    as_tibble()
  
  p <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "black",
      alpha = 0.2) +
    geom_point(
      data = df_lme,
      aes(x = climate_anom, y = value_anom, col = col),
      alpha = 0.5,
      size = 0.5) +
    geom_hline(aes(yintercept = 0), col = "gray") +
    geom_smooth(
      data = df_lme %>% filter(p_value >= 0.05),
      method = "lm",
      se = FALSE,
      aes(x = climate_anom, y = value_anom, group = n, col = col),
      alpha = 0.5,
      linewidth = 0.5,
      linetype = "dashed") +
    geom_smooth(
      data = df_lme %>% filter(p_value < 0.05),
      method = "lm",
      se = FALSE,
      aes(x = climate_anom, y = value_anom, group = n, col = col),
      alpha = 0.5,
      linewidth = 0.5,
      linetype = "solid") +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = climate_anom, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p < 0.05, "solid", "dashed")) +
    theme_classic() +
    xlab(x_lab) +
    ylab(y_lab) +
    labs(title = paste0(y_lab, " ~ ", x_lab, "   lambda = ", lmd, "\nbeta = ", beta, "   p = ", p)) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.y = element_text(color = "black"))
  
  return(p)
}