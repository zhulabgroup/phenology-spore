# source("~/Github/spore_phenology/code/analysis/calc_lm_trend.R")
# df_lm_trend <- calc_lm_trend(df_in = df_ana, metric = "SOS", pct = 0.8)
# source("~/Github/spore_phenology/code/analysis/plot_trend_mapR")
# p_trend_map <- plot_trend_map(df_in = df_lm_trend, metric = "SOS")

# source("~/Github/spore_phenology/code/analysis/calc_lm_trend.R")
# df_lm_trend <- calc_lm_trend(df_in = df_ana, metric = "ln_Ca", pct = 0.8)
# source("~/Github/spore_phenology/code/analysis/plot_trend_map.R")
# p_trend_map <- plot_trend_map(df_in = df_lm_trend, metric = "ln_Ca")

source("~/Github/spore_phenology/code/analysis/calc_lm_trend.R")
df_lm_trend <- calc_lm_trend(df_in = df_ana, metric = "SAS", pct = 0.8)
source("~/Github/spore_phenology/code/analysis/plot_trend_map.R")
p_trend_map <- plot_trend_map(df_in = df_lm_trend, metric = "SAS")
source("~/Github/spore_phenology/code/analysis/extr_color_map.R")
df_lm_trend_col <- extr_color_map(df_in = df_lm_trend, p_map = p_trend_map)
source("~/Github/spore_phenology/code/analysis/filt_station_trend.R")
df_trend_station <- filt_station_trend(df_in = df_lm_trend_col)
source("~/Github/spore_phenology/code/analysis/plot_trend_map_station.R")
p_trend_map_station <- plot_trend_map_station(p_map = p_trend_map, df_in = df_trend_station)

source("~/Github/spore_phenology/code/analysis/plot_trend_station.R")
p_trend_station_list <- list()
for (i in df_trend_station$n) {
  p_trend_station <- plot_trend_station(df_in = df_lm_trend_col, station_n = i, metric = "SAS")
  p_trend_station_list <- c(p_trend_station_list, list(p_trend_station))
}

p_trend_station_composite_c1 <- plot_grid(
  p_trend_station_list[[1]], 
  p_trend_station_list[[2]], 
  ncol = 1,
  rel_heights = c(1, 1))
p_trend_station_composite_r12 <- plot_grid(
  p_trend_station_composite_c1, p_trend_map_station,
  nrow = 1,
  rel_widths = c(1, 3))
p_trend_station_composite_r3 <- plot_grid(
  p_trend_station_list[[3]], p_trend_station_list[[4]], p_trend_station_list[[5]], p_trend_station_list[[6]],
  nrow = 1,
  rel_widths = c(1, 1, 1, 1))
p_trend_station_composite <- plot_grid(
  p_trend_station_composite_r12,
  p_trend_station_composite_r3,
  ncol = 1,
  rel_heights = c(2, 1))