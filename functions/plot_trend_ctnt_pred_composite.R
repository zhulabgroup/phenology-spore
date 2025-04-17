source(str_c(.path$ana_fun, "calc_trend_ctnt.R"))
source(str_c(.path$ana_fun, "calc_trend_station_TS.R"))
source(str_c(.path$ana_fun, "plot_trend_map_TS.R"))
source(str_c(.path$ana_fun, "extr_color_map.R"))
source(str_c(.path$ana_fun, "plot_trend_ctnt_pred.R"))

p_trend_ctnt_pred_list <- list()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  #fit lme model
  m_trend <- calc_trend_ctnt(df_in = df_ana, metric = m_metric, pct = 0.8)
  #acquire col for each station
  df_trend_station <- calc_trend_station_TS(df_in = df_ana, metric = m_metric, pct = 0.8)
  p_trend_map <- plot_trend_map_TS(df_in = df_trend_station, metric = m_metric)
  df_trend_station_col <- extr_color_map(df_in = df_trend_station, p_map = p_trend_map)
  # plot
  p_trend_ctnt_pred <- plot_trend_ctnt_pred(df_in = df_trend_station_col, model = m_trend, metric = m_metric, pct = 0.8)
  p_trend_ctnt_pred_list <- c(p_trend_ctnt_pred_list, list(p_trend_ctnt_pred))
}

title1 <- ggdraw() + draw_label("Ecology Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
title2 <- ggdraw() + draw_label("Public Health Perspective", hjust = 0.5, vjust = 0.5, fontface = "bold")
p_trend_ctnt_pred_composite_r1 <- plot_grid(
  p_trend_ctnt_pred_list[[1]], p_trend_ctnt_pred_list[[3]], p_trend_ctnt_pred_list[[5]], p_trend_ctnt_pred_list[[7]], p_trend_ctnt_pred_list[[9]],
  nrow = 1,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
  label_fontface = "bold",
  label_size = 12,
  rel_widths = c(1, 1, 1, 1, 1))
p_trend_ctnt_pred_composite_r2 <- plot_grid(
  p_trend_ctnt_pred_list[[2]], p_trend_ctnt_pred_list[[4]], p_trend_ctnt_pred_list[[6]], p_trend_ctnt_pred_list[[8]], p_trend_ctnt_pred_list[[10]],
  nrow = 1,
  labels = c("(f)", "(g)", "(h)", "(i)", "(j)"),
  label_fontface = "bold",
  label_size = 12,
  rel_widths = c(1, 1, 1, 1, 1))
p_trend_ctnt_pred_composite <- plot_grid(
  title1,
  p_trend_ctnt_pred_composite_r1,
  title2,
  p_trend_ctnt_pred_composite_r2,
  nrow = 4,
  rel_heights = c(0.1, 1, 0.1, 1))