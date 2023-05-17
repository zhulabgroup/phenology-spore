df <- read_rds(str_c(.path$dat_process, "2023-04-25/spore_dat.rds"))

# station_year combination, measurements > 18, years > 5
df_siteyear <- df %>%
  filter(family == "Total") %>%
  mutate(count = abs(count)) %>% 
  filter(count > 0) %>% 
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  group_by(city, id, year) %>%
  mutate(nobservation = n()) %>%
  filter(nobservation >= 10) %>%
  ungroup() %>%
  group_by(city, id) %>%
  mutate(nyear = length(unique(year))) %>%
  filter(nyear >= 3) %>%
  ungroup() %>%
  dplyr::select(lat, lon, station, city, state, country, id, year, date, count)

# linear interpolation + whittaker smoothing
df_fill <- df_siteyear %>% 
  group_by(lat, lon, station, city, state, country, id) %>%
  complete(date = seq(min(year) %>% paste0("-01-01") %>% ymd(), max(year) %>% paste0("-12-31") %>% ymd(), by = "day")) %>% 
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  mutate(doy = format(date, "%j") %>% as.integer()) %>% 
  ungroup() %>% 
  filter(doy <= 365) %>% 
  left_join(df_siteyear %>% 
              distinct(id, .keep_all = T) %>% 
              mutate(n = row_number()) %>% 
              dplyr::select(-count, -date, -year),
            by = c("lat", "lon", "station", "city", "state", "country", "id")) %>% 
  dplyr::select(lat, lon, station, city, state, country, id, n, year, doy, date, count)
# Whittaker smooth
# Function for smoothing, because ptw::whit1 does not take NA in the time series
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
df_smooth <- df_fill %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  mutate(count_fill = zoo::na.approx(count, maxgap = 7, na.rm = F)) %>% 
  mutate(count_whit = whitfun(count_fill, lambda = 1800)) %>%
  ungroup()

# spline interpolation and smoothing
df_o <- data.frame()
for (i in 1:60) {
  df = df_smooth %>% filter(n == i)
  startyr = min(df_smooth$year)
  count_ts <- df %>% 
    pull(count) %>% 
    ts(frequency = 365, start = c(startyr, 1))
  count_spline <- count_ts %>% 
    TsPP(tsgf = TSGFspline)
  df_spline <- cbind(df, count_spline)
  df_o <- bind_rows(df_o, df_spline)
}

write_rds(df_o, str_c(.path$dat_process, "2023-04-25/fill_whit_spline.rds"))

p_ts_whit_spline <- ggplot(data = df_o, aes(x = date)) +
  geom_point(aes(y = count), col = "gray") +
  geom_line(aes(y = count_spline), col = "red") +
  geom_line(aes(y = count_whit), col = "blue") +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(breaks = "1 year") +
  ylab("count") +
  facet_wrap(. ~ n, ncol = 6, scales = "free_y")
ggsave(
  plot = p_ts_whit_spline,
  filename = "output/figures/p_ts_whit_spline.pdf",
  width = 100,
  height = 150,
  limitsize = F
)

# p_ts_whit_spline_a <- ggplot(data = df_o %>% filter(n %in% 1:12), aes(x = date)) +
#   geom_point(aes(y = count), col = "gray", size = 0.3) +
#   geom_line(aes(y = count_spline), col = "red", size = 0.3) +
#   geom_line(aes(y = count_whit), col = "blue", size = 0.3) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_datetime(breaks = "1 year") +
#   ylab("count") +
#   facet_wrap(. ~ n, ncol = 4, scales = "free_y")
# ggsave(
#   plot = p_ts_whit_spline_a,
#   filename = "output/figures/p_ts_whit_spline_a.pdf",
#   width = 20,
#   height = 10
# )
# p_ts_whit_spline_b <- ggplot(data = df_o %>% filter(n %in% 13:24), aes(x = date)) +
#   geom_point(aes(y = count), col = "gray", size = 0.3) +
#   geom_line(aes(y = count_spline), col = "red", size = 0.3) +
#   geom_line(aes(y = count_whit), col = "blue", size = 0.3) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_datetime(breaks = "1 year") +
#   ylab("count") +
#   facet_wrap(. ~ n, ncol = 4, scales = "free_y")
# ggsave(
#   plot = p_ts_whit_spline_b,
#   filename = "output/figures/p_ts_whit_spline_b.pdf",
#   width = 20,
#   height = 10
# )
# p_ts_whit_spline_c <- ggplot(data = df_o %>% filter(n %in% 25:36), aes(x = date)) +
#   geom_point(aes(y = count), col = "gray", size = 0.3) +
#   geom_line(aes(y = count_spline), col = "red", size = 0.3) +
#   geom_line(aes(y = count_whit), col = "blue", size = 0.3) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_datetime(breaks = "1 year") +
#   ylab("count") +
#   facet_wrap(. ~ n, ncol = 4, scales = "free_y")
# ggsave(
#   plot = p_ts_whit_spline_c,
#   filename = "output/figures/p_ts_whit_spline_c.pdf",
#   width = 20,
#   height = 10
# )
# p_ts_whit_spline_d <- ggplot(data = df_o %>% filter(n %in% 37:48), aes(x = date)) +
#   geom_point(aes(y = count), col = "gray", size = 0.3) +
#   geom_line(aes(y = count_spline), col = "red", size = 0.3) +
#   geom_line(aes(y = count_whit), col = "blue", size = 0.3) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_datetime(breaks = "1 year") +
#   ylab("count") +
#   facet_wrap(. ~ n, ncol = 4, scales = "free_y")
# ggsave(
#   plot = p_ts_whit_spline_d,
#   filename = "output/figures/p_ts_whit_spline_d.pdf",
#   width = 20,
#   height = 10
# )
# p_ts_whit_spline_e <- ggplot(data = df_o %>% filter(n %in% 49:60), aes(x = date)) +
#   geom_point(aes(y = count), col = "gray", size = 0.3) +
#   geom_line(aes(y = count_spline), col = "red", size = 0.3) +
#   geom_line(aes(y = count_whit), col = "blue", size = 0.3) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   scale_x_datetime(breaks = "1 year") +
#   ylab("count") +
#   facet_wrap(. ~ n, ncol = 4, scales = "free_y")
# ggsave(
#   plot = p_ts_whit_spline_e,
#   filename = "output/figures/p_ts_whit_spline_e.pdf",
#   width = 20,
#   height = 10
# )