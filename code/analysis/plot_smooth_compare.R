# plot function for each station
plot_smooth_compare_station <- function(df_raw, i) {
  df_site <- df_raw %>% 
    filter(n == i)
  
  out_gg <- ggplot(data = df_site) +
    geom_line(aes(x = date, y = count), col = "gray") +
    geom_line(aes(x = date, y = count_fillwhit), col = "black", alpha = 0.5) +
    geom_line(aes(x = date, y = count_weiwhit), col = "red", alpha = 0.5) +
    geom_line(aes(x = date, y = count_wavelet), col = "blue", alpha = 0.5) +
    geom_vline(xintercept = (df_site %>% filter(doy == offset))$date, col = "dark green", alpha = 0.5) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    annotation_logticks(sides = "l") +
    theme_bw() +
    ylab(expression("Daily spore concentration (grains m"^-3*")")) +
    scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
    xlab("Date") +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")
  
  return(out_gg)
}

# plot 55 stations on one page
plot_smooth_compare_all <- function(df_raw, n_col, wavelet) {
  out_gg <- ggplot(data = df_raw) +
    geom_line(aes(x = date, y = count), col = "gray") +
    geom_line(aes(x = date, y = count_fillwhit), col = "black", alpha = 0.5) +
    geom_line(aes(x = date, y = count_weiwhit), col = "red", alpha = 0.5) +
    geom_vline(
      aes(xintercept = date),
      data = df_raw %>% group_by(n) %>% filter(doy == offset) %>% ungroup(),
      col = "dark green", alpha = 0.5
      ) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    annotation_logticks(sides = "l") +
    theme_bw() +
    ylab(expression("Daily spore concentration (grains m"^-3*")")) +
    scale_x_datetime(limits = as.POSIXct(c("2003-01-01", "2022-12-31")), breaks = "1 year", date_labels = "%Y") +
    xlab("Date") +
    facet_wrap(. ~ interaction(city, state, n, sep = ", "), ncol = n_col, scales = "free")
  
  if (wavelet == T) {
    out_gg = out_gg +
      geom_line(aes(x = date, y = count_wavelet), col = "blue", alpha = 0.5)
  }
  
  return(out_gg)
}