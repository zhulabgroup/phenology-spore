source("code/analysis/tidy_smoothfillwhit_station.R")
source("code/analysis/tidy_sporeyr_station.R")
source("code/analysis/calc_completeness_stationspyr.R")
source("code/analysis/calc_metrics_stationspyr.R")
source("code/analysis/tidy_gathermetrics.R")
source("code/analysis/calc_trend.R")

ana_sens_l <- function(df_in, lmd) {
  df_smooth_lmd <- tidy_smoothfillwhit_station(df_raw = df_in, n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit)
  
  df_sporeyr = df_smooth_lmd %>% 
    tidy_sporeyr_station()
  
  df_spore_cpltness <- calc_completeness_stationspyr(df_sporeyr, column_name = count_fillwhit)
  
  df_metrics <- calc_metrics_stationspyr(df_completeness = df_spore_cpltness, df_raw = df_sporeyr)
  
  df_metrics_long <- tidy_gathermetrics(df_metrics)
  
  
  
  m_Cp <- calc_trend_metric(df_raw = df_metrics_long, metric = "ln_Cp", pct = 0.8)
  m_AIn <- calc_trend_metric(df_raw = df_metrics_long, metric = "ln_AIn", pct = 0.8)
  m_ASIn <- calc_trend_metric(df_raw = df_metrics_long, metric = "ln_ASIn", pct = 0.8)
  m_SOS <- calc_trend_metric(df_raw = df_metrics_long, metric = "SOS", pct = 0.8)
  m_EOS <- calc_trend_metric(df_raw = df_metrics_long, metric = "EOS", pct = 0.8)
  m_LOS <- calc_trend_metric(df_raw = df_metrics_long, metric = "LOS", pct = 0.8)
  m_SAS <- calc_trend_metric(df_raw = df_metrics_long, metric = "SAS", pct = 0.8)
  m_EAS <- calc_trend_metric(df_raw = df_metrics_long, metric = "EAS", pct = 0.8)
  m_LAS <- calc_trend_metric(df_raw = df_metrics_long, metric = "LAS", pct = 0.8)
  m_A <- calc_trend_metric(df_raw = df_metrics_long, metric = "ln_A", pct = 0.8)
  df_output <- rbind(m_SOS, m_EOS, m_LOS, m_SAS, m_EAS, m_LAS, m_Cp, m_A, m_AIn, m_ASIn) %>% 
    as.data.frame() %>% 
    rename(
      metric = V1,
      beta = V2,
      CI1 = V3,
      CI2 = V4,
      p = V5) %>% 
    mutate(lambda = l)
  rownames(df_output) <- NULL
  
  return(df_output)
}

df_sens_l <- data.frame()
for (l in c(10, seq(50, 500, by = 50))) {
  df <- ana_sens_l(df_wavelet, lmd = l)
  df_sens_l <- df_sens_l %>% rbind(df)
}
metric <- df_sens_l$metric
beta <- as.numeric(df_sens_l$beta)
ci1 <- as.numeric(df_sens_l$CI1)
ci2 <- as.numeric(df_sens_l$CI2)
p <- as.numeric(df_sens_l$p)
l <- as.integer(df_sens_l$lambda)
df_sens_l <- data.frame(metric, beta, ci1, ci2, l)

plot_sens_l <- function(m, df_in) {
  df <- df_in %>% 
    filter(metric == m)
  
  lm <- lm(beta ~ l, data = df)
  signif <- summary(lm)$coef[2, "Pr(>|t|)"] %>% as.numeric() %>% round(5)
  slope <- coef(lm)[2] %>% as.numeric() %>% round(5)
  
  plot <- ggplot(data = df,aes(x = l, y = beta, group = metric, col = metric)) +
    geom_hline(aes(yintercept = 0), col = "gray") +
    #geom_smooth(method = "lm", se = T, col = "dark blue", linetype = ifelse(signif < 0.05, "solid", "dashed")) +
    geom_point(col = "dark red") +
    geom_errorbar(aes(ymin = ci1, ymax = ci2), col = "dark red") +
    scale_x_continuous(breaks = c(10, seq(50, 500, by = 50))) +
    xlab("lambda") +
    ylab("beta") +
    labs(title = paste0(m, "   slope = ", slope, "   p = ", signif)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.y = element_text(color = "black"))
    
  return(plot)
}

plot_list <- list()
for (m in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  p <- plot_sens_l(m, df_in = df_sens_l)
  plot_list <- c(plot_list, list(p))
}
p_sens_l <- plot_grid(plotlist = plot_list, align = "hv", axis = "tblr", ncol = 2, rel_heights = rep(1, 5))

pdf(
  "output/figures/p_sens_l.pdf",
  width = 10,
  height = 8
)
print(p_sens_l)
dev.off()