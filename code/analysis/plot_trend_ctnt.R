# source("code/analysis/calc_trend_station.R")
# source("code/analysis/calc_trend_ctnt.R")
p = 0.8

# beta <- fixef(m_trend)[["year_new"]] %>% as.numeric() %>% round(5)
# CI <- intervals(m_trend, which = "fixed")
# CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% round(5)
# CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% round(5)
# p <- summary(m_trend)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(5)
# result <- c(beta, CI1, CI2, p)
# result
# 19*result

plot_trend_ctnt <- function(df_in, model, metric) {
  beta <- fixef(model)[["year_new"]] %>% as.numeric() %>% round(3)
  p <- summary(model)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(3)
  
  df_lme <- df_in %>% 
    mutate(
      lme_fixed = model$fitted[, 1],
      lme_random = model$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    as_tibble()
  
  df_ci <- ggpredict(model, terms = c("year_new", "n"), type = "re") %>%
    as_tibble()
  
  p <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "black",
      alpha = 0.2) +
    geom_point(
      data = df_lme,
      aes(x = year_new, y = Value, col = col),
      alpha = 0.5,
      size = 0.5) +
    geom_smooth(
      data = df_lme,
      method = "lm",
      se = FALSE,
      aes(x = year_new, y = Value, group = n, col = col),
      alpha = 0.5,
      linewidth = 0.5) +
    scale_color_identity() +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p < 0.05, "solid", "dashed")) +
    theme_classic() +
    xlab("Year") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.y = element_text(color = "black")) +
    theme(
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10)) +
    annotate(
      "text",
      x = min(df_lme$year_new), y = max(df_lme$Value),
      label = paste0("\nbeta = : ", beta,"\np = : ", p),
      hjust = 0, vjust = 0.5, col = "black")
  
  if (metric %in% c("ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    p <- p + scale_y_continuous(labels = scales::math_format(e^.x))
  }
  
  if (metric == "SOS") {
    p <- p + 
      ylab("Start of season (day of spore year)") +
      labs(title = expression(paste(bold("A      "), italic("Advanced SOS"))))
  }
  
  if (metric == "SAS") {
    p <- p + 
      ylab("Start of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("B      "), italic("Advanced SAS"))))
  }
  
  if (metric == "EOS") {
    p <- p + 
      ylab("End of season (day of spore year)") +
      labs(title = expression(paste(bold("C      "), italic("Advanced EOS"))))
  }
  
  if (metric == "EAS") {
    p <- p + 
      ylab("End of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("D      "), italic("Advanced EAS"))))
  }
  
  if (metric == "LOS") {
    p <- p + 
      ylab("Length of season (day of spore year)") +
      labs(title = expression(paste(bold("E      "), italic("Extended LOS"))))
  }
  
  if (metric == "LAS") {
    p <- p + 
      ylab("Length of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("F      "), italic("Extended LAS"))))
  }
  
  if (metric == "ln_A") {
    p <- p + 
      ylab(expression("Amplitude (spores m"^-3*")")) +
      labs(title = expression(paste(bold("G      "), italic("Decreased A"))))
  }
  
  if (metric == "ln_Cp") {
    p <- p + 
      ylab(expression("Peak concentration (spores m"^-3*")")) +
      labs(title = expression(paste(bold("H      "), italic("Decreased Cp"))))
  }
  
  if (metric == "ln_AIn") {
    p <- p + 
      ylab(expression("Annual integral (spores m"^-3*" days)")) +
      labs(title = expression(paste(bold("I      "), italic("Decreased AIn"))))
  }
  
  if (metric == "ln_ASIn") {
    p <- p + 
      ylab(expression("Allergy season integral (spores m"^-3*" days)")) +
      labs(title = expression(paste(bold("J      "), italic("Decreased ASIn"))))
  }
  
  return(p)
}






title1 <- ggdraw() + draw_label("Ecology Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
title2 <- ggdraw() + draw_label("Public Health Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
plot_list <- list()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  df_trend_station <- calc_trend_station(df_in = df_ana, metric = m_metric, pct = p)
  m_trend <- calc_trend_ctnt(df_in = df_trend_station, metric = m_metric, pct = p)
  p_trend_ctnt <- plot_trend_ctnt(df_in = df_trend_station, model = m_trend, metric = m_metric)
  plot_list <- c(plot_list, list(p_trend_ctnt))
}
p_trend_r1 <- plot_grid(plot_list[[1]], plot_list[[3]], plot_list[[5]], plot_list[[7]], plot_list[[9]],
                        nrow = 1,
                        rel_widths = c(1, 1, 1, 1, 1))
p_trend_r2 <- plot_grid(plot_list[[2]], plot_list[[4]], plot_list[[6]], plot_list[[8]], plot_list[[10]],
                        nrow = 1,
                        rel_widths = c(1, 1, 1, 1, 1))
p_trend <- plot_grid(title1,
                     p_trend_r1,
                     title2,
                     p_trend_r2,
                     nrow = 4,
                     rel_heights = c(0.1, 1, 0.1, 1))
