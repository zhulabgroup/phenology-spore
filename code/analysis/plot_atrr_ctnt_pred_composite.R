source("~/Github/spore_phenology/code/analysis/calc_trend_station.R")
source("~/Github/spore_phenology/code/analysis/plot_trend_map.R")
source("~/Github/spore_phenology/code/analysis/extr_color_map.R")

source("~/Github/spore_phenology/code/analysis/calc_atrr_ctnt.R")
source("~/Github/spore_phenology/code/analysis/plot_atrr_ctnt_pred.R")

p_atrr_ctnt_pred_list <- list()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  #acquire col for each station
  df_trend_station <- calc_trend_station(df_in = df_ana, metric = m_metric, pct = 0.8)
  p_trend_map <- plot_trend_map(df_in = df_trend_station, metric = m_metric)
  df_trend_station_col <- extr_color_map(df_in = df_trend_station, p_map = p_trend_map)
  for (cli in c("MAT", "TAP")) {
    # fit lme model using climate data
    m_lme_climate <- calc_atrr_ctnt(df_in = df_trend_station_col, metric = m_metric, cli_vrb = cli, pct = 0.8)
    p_atrr_ctnt_pred <- plot_atrr_ctnt_pred(model = m_lme_climate, df_in = df_trend_station_col, metric = m_metric, cli_vrb = cli, pct = 0.8)
    p_atrr_ctnt_pred_list <- c(p_atrr_ctnt_pred_list, list(p_atrr_ctnt_pred))
  }
}

p_atrr_ctnt_pred_composite <- p_atrr_ctnt_pred_list[[1]] + p_atrr_ctnt_pred_list[[2]] + p_atrr_ctnt_pred_list[[4]] + p_atrr_ctnt_pred_list[[18]] +
  p_atrr_ctnt_pred_list[[9]] + p_atrr_ctnt_pred_list[[10]] + p_atrr_ctnt_pred_list[[12]] + p_atrr_ctnt_pred_list[[20]] +
  plot_layout(nrow = 2) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(plot.title = element_text(size = 12)))
