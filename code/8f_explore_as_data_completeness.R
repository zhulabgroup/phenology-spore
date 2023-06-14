df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))
df_c <- left_join(df_smooth, df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
df_completeness <- df_c %>% 
  mutate(date_new = as.Date(date_new)) %>% 
  mutate(month_new = format(date_new, "%m")) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, observ_pct, year_new, month_new) %>% 
  mutate(observ_pct_mon = sum(!is.na(count_whit))/days_in_month(date_new)) %>% 
  ungroup()




df_plot <- tibble(
  n = 1:60,
  a = vector("list", 60),
  b = vector("list", 60)
)


for (i in c(1:60)) {
  

  df <- df_completeness %>% 
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
  
  p_a <- ggplot() +
    # geom_point(
    #   data = df,
    #   aes(x = date, y = count), col = "gray"
    # ) +
    geom_vline(
      xintercept = (df %>% filter(doy == offset))$date,
      col = "blue"
    ) +
    geom_ribbon(
      data = data_las,
      aes(group = year_new, x = date, ymin = 0, ymax = count_whit),
      fill = "orange",
      alpha = 0.4
    ) +
    geom_hline(
      yintercept = 6500,
      col = "black",
      alpha = 0.5,
      linetype = "dashed") +
    geom_point(
      data = df %>% 
        filter(date == peak_date_old) %>% 
        filter(peak_check == 1),
      aes(x = date, y = count_whit),
      col = "red"
    ) +
    geom_point(
      data = df %>%
        filter(date == peak_date_old) %>% 
        filter(peak_check == 0),
      aes(x = date, y = count_whit),
      col = "red",
      shape = 1
    ) +
    geom_point(
      data = df %>% 
        drop_na(sas) %>% 
        filter(date == sas_date_old),
      aes(x = date, y = count_whit),
      col = "orange"
    ) +
    geom_point(
      data = df %>% 
        drop_na(eas) %>% 
        filter(date == eas_date_old),
      aes(x = date, y = count_whit),
      col = "orange"
    ) +
    geom_line(
      data = df,
      aes(x = date, y = count_whit)
    ) +
    scale_y_continuous(
      trans = scales::log_trans(),
      breaks = scales::trans_breaks("log", function(x) exp(x)),
      labels = scales::trans_format("log", scales::math_format(e^.x))
    ) +
    ylab("concentration (per cubic meter)") +
    scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")
  
  p_b <- ggplot(data = df, aes(x = month_new, y = year_new, fill = observ_pct_mon)) +
    geom_tile(width = 0.9, height = 0.9) +
    geom_tile(data = df[df$observ_pct_mon == 0, ], width = 0.9, height = 0.9, fill = "white", color = "dark green") +
    geom_text(data = df, aes(x = -Inf, y = year_new, label = paste0(round(observ_pct * 100, 1), "%")), color = "black", size = 3, hjust = -0.3) +
    scale_fill_gradient(low = "white", high = "dark green") +
    scale_x_discrete(expand = c(0.3, 0)) +
    scale_y_continuous(breaks = unique(df$year_new), expand = c(0, 0), limits = c(min(df$year_new) - 1, max(df$year_new) + 1)) +
    labs(x = "month", y = "year", fill = "Completeness") +
    ggtitle("Data Completeness by Month") +
    theme_classic()
  
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b

}


pdf(
  "output/figures/p_as_data_completeness.pdf",
  width = 12,
  height = 4
)
for (i in 1:60) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(4,3))
}
dev.off()

