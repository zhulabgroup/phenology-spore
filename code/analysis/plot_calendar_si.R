df_full <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_spore_fulldate.rds"))
df_ana_short <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_ana_short.rds"))
source("~/Github/spore_phenology/code/analysis/calc_calendar.R")
source("~/Github/spore_phenology/code/analysis/plot_calendar.R")

df_calendar <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "lon")
p_calendar_suppl <- plot_calendar(df_calendar)