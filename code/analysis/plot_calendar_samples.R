df_full <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_spore_fulldate.rds"))
df_ana_short <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_ana_short.rds"))
source("~/Github/spore_phenology/code/analysis/calc_calendar.R")
source("~/Github/spore_phenology/code/analysis/plot_calendar.R")

df_calendar_a <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "ecoregion")
p_calendar_a <- plot_calendar(df_calendar_a)

df_calendar_b <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "mm")
p_calendar_b <- plot_calendar(df_calendar_b)

legend_grob <- get_legend(p_calendar_a)
p_calendar_r1 <- plot_grid(
  p_calendar_a + theme(legend.position = 'none'), p_calendar_b + theme(legend.position = 'none'),
  ncol = 2,
  labels = c("A", "B"), label_fontface = "plain", label_size = 12,
  align = "v",
  rel_widths = c(1, 1))
p_calendar <- plot_grid(p_calendar_r1, legend_grob, ncol = 1, rel_heights = c(4, 1))