df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_smooth_100.rds"))
df_ana_short <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_ana_short_as2000.rds"))

df_c <- left_join(df_smooth, df_ana_short, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))

p_as_list <- list()
for (i in c(1:55)) {
  df <- df_c %>% 
    filter(n == i)
  data_las <- df %>% 
    drop_na(las)
  if (nrow(data_las) > 0) {
    data_las <- data_las %>%
      group_by(year_new) %>%
      filter(doy_new %in% sas:eas) %>%
      ungroup()
  } else {
    data_las <- data_las
  }
  
  p_as <- ggplot() +
    # geom_point(
    #   data = df,
    #   aes(x = date, y = count), col = "gray"
    # ) +
    geom_vline(
      xintercept = (df %>% filter(doy == offset))$date,
      col = "blue") +
    geom_ribbon(
      data = data_las,
      aes(group = year_new, x = date, ymin = 0, ymax = count_fillwhit),
      fill = "orange",
      alpha = 0.4
    ) +
    geom_hline(
      yintercept = 2000,
      col = "black",
      alpha = 0.5,
      linetype = "dashed") +
    geom_point(
      data = df %>% 
        filter(date == peak_date_old),
      aes(x = date, y = count_fillwhit),
      col = "red") +
    geom_point(
      data = df %>% 
        drop_na(sas) %>% 
        filter(date == sas_date_old),
      aes(x = date, y = count_fillwhit),
      col = "orange") +
    geom_point(
      data = df %>% 
        drop_na(eas) %>% 
        filter(date == eas_date_old),
      aes(x = date, y = count_fillwhit),
      col = "orange") +
    geom_line(
      data = df,
      aes(x = date, y = count_fillwhit)) +
    scale_y_continuous(
      trans = "log10",
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ylab("concentration (per cubic meter)") +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), scales = "free_y")
  
  p_as_list <- c(p_as_list, list(p_as))
}

pdf(
  str_c(.path$out_fig, "alllergy_season_2000.pdf"),
  width = 6,
  height = 6*0.618)
for (i in c(1:55)){
  print(p_as_list[[i]])
}
dev.off()