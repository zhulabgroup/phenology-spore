# manipulate lambda for all stations
source("tidy_smoothfillwhit_station.R")
source("tidy_smoothweiwhit_station.R")
source("plot_smooth_compare.R")
df_plot <- tibble(
  n = 1:30,
  a = vector("list", 30)
)
for (l in c(1:30)) {
  lmd = l * 100
  df_smooth_lmd <- df_wavelet %>%
    tidy_smoothfillwhit_station(n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit) %>%
    tidy_smoothweiwhit_station(n_gap = 14, lambda = lmd, column_name = count, new_column_name = count_weiwhit)
  gg_smooth_compare_all <- plot_smooth_compare_all(df_smooth_lmd, 8, wavelet = F)
  df_plot$a[[l]] <- gg_smooth_compare_all
}
pdf(
  "~/Github/spore_phenology/output/figures/smooth_compare/p_compare_smooth.pdf",
  width = 8*8,
  height = 8*.618*7
)
for (i in c(1:30)) {
  print(df_plot$a[[i]])
}
dev.off()



# manipulate lambda for all stations
source("tidy_smoothfillwhit_station.R")
source("plot_smooth_compare.R")
for (i in c(1:55)) {
  
  df_plot <- list()
  
  for (l in c(10, seq(50, 500, by = 50))) {
    lmd = l
    df_smooth_lmd <- df_wavelet %>%
      filter(n == i) %>%
      tidy_smoothfillwhit_station(n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit)
    gg_smooth_compare_station <- plot_smooth_compare_all(df_smooth_lmd, 1, wavelet = F)
    df_plot <- c(df_plot, list(gg_smooth_compare_station + labs(title = paste0("lambda = ", lmd))))
  }
  
  pdf(
    str_c("~/Github/spore_phenology/output/figures/smooth_compare/p_sens_l_station", i, ".pdf"),
    width = 8,
    height = 8*.618
  )
  for (j in c(1:11)) {
    print(df_plot[[j]])
  }
  dev.off()
  
}