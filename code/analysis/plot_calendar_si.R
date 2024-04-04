df_full <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_spore_fulldate.rds"))
df_ana_short <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_ana_short.rds"))
source("~/Github/spore_phenology/code/analysis/calc_calendar.R")
source("~/Github/spore_phenology/code/analysis/plot_calendar.R")
df_calendar <- calc_calendar(df_in = df_full, df_meta = df_ana_short, y_label = "lon")
p_calendar_suppl <- plot_calendar(df_in = df_calendar, y_label = "lon")

source("~/Github/spore_phenology/code/analysis/summ_miss_station_raw_data.R")
df_miss <- df_full %>% 
  summ_miss_stationyear_raw_data(column_name = count) %>% 
  left_join(
    df_ana_short %>% 
      group_by(lat, lon, station, city, state, country, id, n) %>% 
      summarise(
        tap_mean = mean(tap),
        mat_mean = mean(mat)),
    by = c("lat", "lon", "station", "city", "state", "country", "id", "n")) %>% 
  mutate(ylab = paste0(city, ", ", state, " (",round(-lon), "° W)")) %>% 
  mutate(order = lon)

p_calendar_suppl <- p_calendar_suppl +
  geom_text(
    data = df_miss,
    aes(x = 400, y = reorder(ylab, order), label = paste(round((1 - miss), 4) * 100, "%")),
    vjust = 0.5, hjust = 1,
    color = "black")
