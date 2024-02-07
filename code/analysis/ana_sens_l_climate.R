source("code/analysis/tidy_smoothfillwhit_station.R")
source("code/analysis/tidy_sporeyr_station.R")
source("code/analysis/calc_completeness_stationspyr.R")
source("code/analysis/calc_metrics_stationspyr.R")
source("code/analysis/tidy_gathermetrics.R")
df_daymet_annual <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_daymet_annual.rds"))
source("code/analysis/calc_anom.R")
source("code/analysis/calc_lm_anom.R")
source("code/analysis/calc_lme_anom.R")
source("code/analysis/plot_lme_anom.R")

tidy_l_climate <- function(df_in, lmd, df_climate) {
  df_smooth_lmd <- df_in %>%
    tidy_smoothfillwhit_station(n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit)
  
  df_sporeyr = df_smooth_lmd %>% 
    tidy_sporeyr_station()
  
  df_spore_cpltness <- calc_completeness_stationspyr(df_sporeyr, column_name = count_fillwhit)
  
  df_metrics <- calc_metrics_stationspyr(df_completeness = df_spore_cpltness, df_raw = df_sporeyr)
  
  df_metrics_long <- tidy_gathermetrics(df_metrics)
  
  df <- right_join(df_climate, df_metrics_long, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
  
  return(df)
}

ana_sens_l_climate <- function(df_in, lmd, df_climate, metric, pct, x_lab, y_lab) {
  df_ana <- tidy_l_climate(df_in = df_in, lmd = lmd, df_climate = df_climate)
  color_palette <- colorRampPalette(colors = rainbow(55))
  df_ana = df_ana %>%
    mutate(col = color_palette(55)[n])
  df_anom <- calc_anom(df_raw = df_ana, metric = metric, pct = pct)
  df_lm <- calc_lm_anom(df_in = df_anom, metric = metric, pct = pct, x = x_lab)
  m_lme <- calc_lme_anom(df_in = df_lm)
  p <- plot_lme_anom(df_in = df_lm, m = m_lme, x_lab = x_lab, y_lab = y_lab, lmd = lmd)
  return(p)
}

l = 100
m = "SOS"
x_lab = "MAT_anom"
y_lab = "SOS_anom"
p <- ana_sens_l_climate(df_in = df_wavelet, lmd = l, df_climate = df_daymet_annual, metric = m, pct = 0.8, x_lab = x_lab, y_lab = y_lab)

plot_lme_anom_list <- list()
for (m in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_A", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  plot_list <- list()
  y_lab = paste0(m, "_anom")
  
  for (l in c(10, 50, 100, 150, 200, 250, 300)) {
    for (x_lab in c("MAT_anom", "TAP_anom")) {
      p <- ana_sens_l_climate(df_in = df_wavelet, lmd = l, df_climate = df_daymet_annual, metric = m, pct = 0.8, x_lab = x_lab, y_lab = y_lab)
      plot_list <- c(plot_list, list(p))
    }
  }
  
  p_lme_anom <- plot_grid(plotlist = plot_list, align = "hv", axis = "tblr", ncol = 2, rel_heights = rep(1, 7))
  plot_lme_anom_list <- c(plot_lme_anom_list, list(p_lme_anom))
}

pdf(
  "output/figures/p_sens_lme_anom.pdf",
  width = 5 * 2,
  height = 5 * 7
)
for (i in c(1:10)) {
  print(plot_lme_anom_list[[i]])
}
dev.off()
