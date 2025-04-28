df_full <- read_rds(str_c(.path$dat_process, "dat_spore_fulldate.rds"))
df_ana_short <- read_rds(str_c(.path$dat_process, "dat_ana_short_as4605.rds"))
source(str_c(.path$ana_fun, "calc_calendar.R"))
source(str_c(.path$ana_fun, "plot_calendar.R"))

df_calendar_a <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "ecoregion")
p_calendar_a <- plot_calendar(df_calendar_a, y_label = "ecoregion")

df_calendar_b <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "mm")
p_calendar_b <- plot_calendar(df_calendar_b, y_label = "mm")

p_calendar <- (p_calendar_a + p_calendar_b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p_calendar <- p_calendar +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 12, face = "bold")) &
  plot_annotation(tag_prefix = "(", tag_suffix = ")")
