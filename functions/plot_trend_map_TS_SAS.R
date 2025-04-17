source(str_c(.path$ana_fun, "calc_trend_station_TS.R"))
df_trend_station <- calc_trend_station_TS(df_in = df_ana, metric = "SAS", pct = 0.8)
source(str_c(.path$ana_fun, "plot_trend_map_TS.R"))
p_trend_map_TS_SAS <- plot_trend_map_TS(df_in = df_trend_station, metric = "SAS") +
  guides(size = "none")