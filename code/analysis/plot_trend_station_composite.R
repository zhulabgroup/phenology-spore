source("~/Github/spore_phenology/code/analysis/calc_lm_trend.R")
df_lm_trend <- calc_lm_trend(df_in = df_ana, metric = "SOS", pct = 0.8)
source("~/Github/spore_phenology/code/analysis/plot_map_trend.R")
p_map_trend <- plot_map_trend(df_in = df_lm_trend, metric = "SOS")

source("~/Github/spore_phenology/code/analysis/calc_lm_trend.R")
df_lm_trend <- calc_lm_trend(df_in = df_ana, metric = "SAS", pct = 0.8)
source("~/Github/spore_phenology/code/analysis/plot_map_trend.R")
p_map_trend <- plot_map_trend(df_in = df_lm_trend, metric = "SAS")
p_map_trend
