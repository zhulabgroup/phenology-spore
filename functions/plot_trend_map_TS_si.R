source(str_c(.path$ana_fun, "calc_trend_station_TS.R"))
source(str_c(.path$ana_fun, "plot_trend_map_TS.R"))

p_trend_map_TS_list <- list()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  df_trend_station <- calc_trend_station_TS(df_in = df_ana, metric = m_metric, pct = 0.8)
  p_trend_map_TS <- plot_trend_map_TS(df_in = df_trend_station, metric = m_metric)
  p_trend_map_TS_list <- c(p_trend_map_TS_list, list(p_trend_map_TS))
}

p_trend_map_TS_si <- cowplot::plot_grid(
  p_trend_map_TS_list[[1]] + guides(size = "none", color = "none"), p_trend_map_TS_list[[2]] + guides(size = "none", color = "none"),
  p_trend_map_TS_list[[3]] + guides(size = "none", color = "none"), p_trend_map_TS_list[[4]] + guides(size = "none", color = "none"),
  p_trend_map_TS_list[[5]] + guides(size = "none", color = "none"), p_trend_map_TS_list[[6]] + guides(size = "none", color = "none"),
  p_trend_map_TS_list[[7]] + guides(size = "none", color = "none"), p_trend_map_TS_list[[8]] + guides(size = "none", color = "none"),
  p_trend_map_TS_list[[9]] + guides(size = "none", color = "none"), p_trend_map_TS_list[[10]] + guides(size = "none", color = "none"),
  ncol = 2,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)"),
  label_fontface = "bold",
  label_size = 12
)
