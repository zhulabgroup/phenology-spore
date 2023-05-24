df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset.rds"))

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

i = 33

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
  geom_point(data = df, aes(x = doy, y = count, col = year, alpha = 0.1, group = year)) +
  guides(alpha = "none") +
  geom_point(aes(y = count_md), col = "purple") +
  geom_line(aes(y = count_md_whit), col = "red") +
  geom_vline(xintercept = offset, col = "blue") +
  geom_vline(xintercept = c(91, 304), col = "dark green") +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ylab("count") +
  labs(color = paste0("lambda = ", l, "\noffset = ", offset, "\nyear")) +
  facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y") 

p_b <- ggplot(data = df) +
  geom_point(aes(x = date, y = count), col = "gray") +
  geom_vline(xintercept = (df %>% filter(doy == offset))$date, col = "dark blue") +
  # geom_ribbon(
  #   data = df %>% filter(doy %in% 91:304) %>% filter(integral_check == 1),
  #   aes(group = year, x = date, ymin = 0, ymax = count_whit),
  #   fill = "dark green",
  #   alpha = 0.4
  #     ) +
  # geom_ribbon(
  #   data = df %>% filter(doy %in% 91:304) %>% filter(integral_check == 0),
  #   aes(group = year, x = date, ymin = 0, ymax = count_whit),
  #   fill = "dark green",
  #   alpha = 0.1
  # ) +
  geom_ribbon(
    data = df %>% group_by(year_new) %>% filter(doy_new %in% sos:eos) %>% ungroup() %>% filter(season_check == 1),
    aes(group = year_new, x = date, ymin = 0, ymax = count_whit),
    fill = "dark red",
    alpha = 0.4
  ) +
  geom_ribbon(
    data = df %>% group_by(year_new) %>% filter(doy_new %in% sos:eos) %>% ungroup() %>% filter(season_check == 0),
    aes(group = year_new, x = date, ymin = 0, ymax = count_whit),
    fill = "dark red",
    alpha = 0.1
  ) +
  geom_line(aes(x = date, y = count_whit)) +
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
  ylab("count") +
  scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
  facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")

df_plot$a[[i]] <- p_a
df_plot$b[[i]] <- p_b