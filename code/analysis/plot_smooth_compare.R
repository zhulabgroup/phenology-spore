# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/smooth_compare.rds"))
# df_ln_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/smooth_compare_ln.rds"))


# plot for each station
plot_smooth_site <- function(df_raw, site) {
  df_site <- df_raw %>% 
    filter(n == i)
  
  out_gg <- ggplot(data = df_raw) +
    geom_line(aes(x = date, y = count), col = "gray") +
    geom_line(aes(x = date, y = count_fillwhit), col = "black", alpha = 0.5) +
    geom_line(aes(x = date, y = count_weiwhit), col = "red", alpha = 0.5) +
    geom_line(aes(x = date, y = count_wavelet), col = "blue", alpha = 0.5) +
    geom_vline(xintercept = (df %>% filter(doy == offset))$date, col = "dark green", alpha = 0.5) +
    scale_y_continuous(
      trans = scales::log_trans(),
      breaks = scales::trans_breaks("log", function(x) exp(x)),
      labels = scales::trans_format("log", scales::math_format(e^.x))
    ) +
    ylab("count") +
    scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")
}