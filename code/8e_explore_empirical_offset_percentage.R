df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_percentage.rds"))
df_c <- left_join(df_smooth, df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))

whitfun <- function(x, lambda) {
  max_id <- 0
  done <- F
  while (!done) {
    min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
    if (min_id == Inf) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
      if (max_id == Inf) {
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}

df_plot <- tibble(
  n = 1:60,
  a = vector("list", 60),
  b = vector("list", 60)
)

for (i in c(1:60)) {

  df <- df_c %>% 
    filter(n == i)
  
  l <- df %>% 
    head(1) %>% 
    pull(lambda)
  
  offset <- df %>% 
    head(1) %>% 
    pull(offset)
  
  city <- df %>% 
    head(1) %>% 
    pull(city)
  
  state <- df %>% 
    head(1) %>% 
    pull(state)
  
  df_median <- df %>% 
    group_by(doy) %>% 
    summarise(count_md = median(count, na.rm = T)) %>% 
    mutate(count_md = ifelse(is.nan(count_md), NA, count_md)) %>% 
    mutate(count_md_fill = zoo::na.approx(count_md, maxgap = 7, na.rm = F)) %>% 
    mutate(count_md_whit = whitfun(count_md_fill, lambda = l))
  
  p_a <- ggplot(data = df_median, aes(x = doy)) +
    geom_point(data = df %>% drop_na(count_whit), aes(x = doy, y = count, col = year, alpha = 0.1, group = year)) +
    guides(alpha = "none") +
    geom_point(aes(y = count_md), col = "purple") +
    geom_line(aes(y = count_md_whit), col = "red") +
    geom_vline(xintercept = offset, col = "blue") +
    scale_y_continuous(
      trans = scales::log_trans(),
      breaks = scales::trans_breaks("log", function(x) exp(x)),
      labels = scales::trans_format("log", scales::math_format(e^.x))
    ) +
    ylab("concentration (per cubic meter)") +
    labs(color = paste0("lambda = ", l, "\noffset = ", offset, "\nyear")) +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y") 
  
  p_b <- ggplot() +
    geom_point(
      data = df,
      aes(x = date, y = count), col = "gray"
      ) +
    geom_vline(
      xintercept = (df %>% filter(doy == offset))$date,
      col = "dark blue"
      ) +
    geom_ribbon(
    data = df %>% drop_na(los) %>% group_by(year_new) %>% filter(doy_new %in% sos:eos) %>% ungroup() %>% filter(season_check == 1),
    aes(group = year_new, x = date, ymin = 0, ymax = count_whit),
    fill = "dark red",
    alpha = 0.4
    ) +
    geom_ribbon(
      data = df %>% drop_na(los) %>% group_by(year_new) %>% filter(doy_new %in% sos:eos) %>% ungroup() %>% filter(season_check == 0),
      aes(group = year_new, x = date, ymin = 0, ymax = count_whit),
      fill = "dark red",
      alpha = 0.1
    ) +
    geom_line(
      data = df %>% filter(integral_check == 1),
      aes(group = year_new, x = date, y = count_whit)
      ) +
    geom_line(
      data = df %>% filter(integral_check == 0),
      aes(group = year_new, x = date, y = count_whit),
      col = "dark green"
      ) +
    geom_point(
      data = df %>% group_by(year_new) %>% filter(date == peak_date_old) %>% filter(peak_check == 1),
      aes(x = date, y = count_whit),
      col = "red",
    ) +
    geom_point(
      data = df %>% group_by(year_new) %>% filter(date == peak_date_old) %>% filter(peak_check == 0),
      aes(x = date, y = count_whit),
      col = "red",
      shape = 1
    ) +
    scale_y_continuous(
      trans = scales::log_trans(),
      breaks = scales::trans_breaks("log", function(x) exp(x)),
      labels = scales::trans_format("log", scales::math_format(e^.x))
    ) +
    ylab("concentration (per cubic meter)") +
    scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
    facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")
  
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b
}

pdf(
  "output/figures/p_offset_80percentage_a.pdf",
  width = 12,
  height = 4
)
for (i in c(3, 6, 17, 22, 29, 40, 11, 50)) {
  p <- grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
                    ncol = 2,
                    widths = c(3,4)
  )
}
dev.off()

pdf(
  "output/figures/p_offset_80percentage_b.pdf",
  width = 12,
  height = 4
)
for (i in c(8, 18, 19, 24, 26, 27, 28, 30, 31, 34, 37, 38, 44, 45, 49, 51, 57, 33, 35, 36, 39, 41, 52, 58, 60)) {
  p <- grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
                    ncol = 2,
                    widths = c(3,4)
  )
}
dev.off()

pdf(
  "output/figures/p_offset_80percentage_c.pdf",
  width = 12,
  height = 4
)
for (i in c(12, 42, 53, 1, 2, 4, 5, 7, 9, 10, 13, 14, 15, 16, 20, 21, 23, 25, 32, 43, 46, 47, 48, 54, 55, 56, 59)) {
  p <- grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
                    ncol = 2,
                    widths = c(3,4)
  )
}
dev.off()

pdf(
  "output/figures/p_offset_80percentage.pdf",
  width = 12,
  height = 4
)
for (i in 1:60) {
  p <- grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
                    ncol = 2,
                    widths = c(3,4)
  )
}
dev.off()
