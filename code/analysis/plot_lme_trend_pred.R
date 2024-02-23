pct = 0.8
df_in <- df_ana

metric = "ln_ASIn"
calc_trend_ctnt <- function(df_in, metric, pct) {
  df <- df_in %>%
    filter(Metric == metric) %>%
    filter(cpltness >= pct) %>%
    drop_na(Value) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset) %>%
    filter(n() >= 5) %>%
    mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
    ungroup()
  
  tryCatch({
    m_ctnt <- lme(
      Value ~ year_new,
      data = df,
      random = ~ (-1+year_new) | n + 1|n)
    return(m_ctnt)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m_ctnt <- lme(
      Value ~ year_new,
      data = df,
      random = ~ year_new | n,
      control = lmeControl(opt = "optim"))
    return(m_ctnt)
  })
}
m_trend <- calc_trend_ctnt(df_in = df_ana, metric = "SOS", pct = 0.8)
plot_lme_trend <- function(df_in, m, metric) {
  beta_value <- fixef(m_trend)[["year_new"]] %>% as.numeric() %>% round(3)
  p_value <- summary(m_trend)$tTable[["year_new", "p-value"]] %>% as.numeric() %>% round(3)
  station_slope <- coef(m_trend) %>% 
    rename("n_intercept" = "(Intercept)") %>% 
    rename(n_slope = year_new)
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
      lme_fixed = m_trend$fitted[, 1],
      lme_random = m_trend$fitted[, 2]
    ) %>%
    mutate(n = as.character(n)) %>%
    as_tibble() %>% 
    left_join(station_slope, by = "n")
  df_ci <- ggpredict(m_trend, terms = c("year_new", "n"), type = "re") %>%
    as_tibble()
  
  p <- ggplot() +
    geom_ribbon(
      data = df_ci,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "grey80") +
    geom_point(
      data = df_lme,
      aes(x = year_new, y = Value, col = n_slope),
      alpha = 0.5,
      size = 0.5) +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_random, group = n, col = n_slope),
      alpha = 0.5,
      linewidth = 0.3) +
    scale_color_gradient2(
      low = "red", mid= "white", high = "blue", midpoint = 0,
      breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
      labels = function(x) {
        ifelse(x == 0, "0", paste0(x, "\u00B3"))
      },
      name = "Temporal trend of\nln(peak concentration)") +
    geom_line(
      data = df_lme,
      aes(x = year_new, y = lme_fixed),
      col = "black",
      linewidth = 1,
      linetype = ifelse(p_value < 0.05, "solid", "dashed")) +
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
      label = bquote(
        atop(italic(beta) == .(beta_value), italic(p) == .(p_value))
      ),
      hjust = 0, vjust = 1, col = "black", fontface = "plain")
  
  if (metric %in% c("ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")) {
    p <- p + scale_y_continuous(labels = scales::math_format(e^.x))
  }
  
  if (metric == "SOS") {
    p <- p + 
      ylab("Start of season (day of spore year)") +
      labs(title = expression(paste(bold("A      "), "Advanced SOS")))
  }
  
  if (metric == "SAS") {
    p <- p + 
      ylab("Start of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("B      "), "Advanced SAS")))
  }
  
  if (metric == "EOS") {
    p <- p + 
      ylab("End of season (day of spore year)") +
      labs(title = expression(paste(bold("C      "), "Advanced EOS")))
  }
  
  if (metric == "EAS") {
    p <- p + 
      ylab("End of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("D      "), "Advanced EAS")))
  }
  
  if (metric == "LOS") {
    p <- p + 
      ylab("Length of season (day of spore year)") +
      labs(title = expression(paste(bold("E      "), "Extended LOS")))
  }
  
  if (metric == "LAS") {
    p <- p + 
      ylab("Length of allergy season (day of spore year)") +
      labs(title = expression(paste(bold("F      "), "Extended LAS")))
  }
  
  if (metric == "ln_A") {
    p <- p + 
      ylab(expression("Amplitude (grains m"^-3*")")) +
      labs(title = expression(paste(bold("G      "), "Decreased A")))
  }
  
  if (metric == "ln_Cp") {
    p <- p + 
      ylab(expression("Peak concentration (grains m"^-3*")")) +
      labs(title = expression(paste(bold("H      "), "Decreased Cp")))
  }
  
  if (metric == "ln_AIn") {
    p <- p + 
      ylab(expression("Annual integral (grains m"^-3*" days)")) +
      labs(title = expression(paste(bold("I      "), "Decreased AIn")))
  }
  
  if (metric == "ln_ASIn") {
    p <- p + 
      ylab(expression("Allergy season integral (grains m"^-3*" days)")) +
      labs(title = expression(paste(bold("J      "), "Decreased ASIn")))
  }
  
  return(p)
}
