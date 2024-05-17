# source("~/Github/spore_phenology/code/analysis/calc_trend_station.R")
# df_trend_station <- calc_trend_station(df_in = df_ana, metric = "SOS", pct = 0.8)
# source("~/Github/spore_phenology/code/analysis/plot_trend_mapR")
# p_trend_map <- plot_trend_map(df_in = df_trend_station, metric = "SOS")

# source("~/Github/spore_phenology/code/analysis/calc_trend_station.R")
# df_trend_station <- calc_trend_station(df_in = df_ana, metric = "ln_Ca", pct = 0.8)
# source("~/Github/spore_phenology/code/analysis/plot_trend_map.R")
# p_trend_map <- plot_trend_map(df_in = df_trend_station, metric = "ln_Ca")

source("~/Github/spore_phenology/code/analysis/calc_trend_station.R")
df_trend_station <- calc_trend_station(df_in = df_ana, metric = "SAS", pct = 0.8)
source("~/Github/spore_phenology/code/analysis/plot_trend_map.R")
p_trend_map <- plot_trend_map(df_in = df_trend_station, metric = "SAS")
source("~/Github/spore_phenology/code/analysis/extr_color_map.R")
df_trend_station_col <- extr_color_map(df_in = df_trend_station, p_map = p_trend_map)
source("~/Github/spore_phenology/code/analysis/filt_station_trend.R")
df_trend_station_sample <- filt_station_trend(df_in = df_trend_station_col)
source("~/Github/spore_phenology/code/analysis/plot_trend_map_station.R")
p_trend_map_station <- plot_trend_map_station(p_map = p_trend_map, df_in = df_trend_station_sample)

source("~/Github/spore_phenology/code/analysis/plot_trend_station.R")
p_trend_station_list <- list()
for (i in df_trend_station_sample$n) {
  p_trend_station <- plot_trend_station(df_in = df_trend_station_col, station_n = i, metric = "SAS")
  p_trend_station_list <- c(p_trend_station_list, list(p_trend_station))
}

layout <- "
BAAA
CAAA
DEFG
"
p_trend_station_composite <- p_trend_map_station + p_trend_station_list[[1]] + p_trend_station_list[[2]] + 
  p_trend_station_list[[3]] + p_trend_station_list[[4]] + p_trend_station_list[[5]] + p_trend_station_list[[6]] +
  plot_layout(design = layout)