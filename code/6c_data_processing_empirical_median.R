# df_spore <- read_rds(str_c(.path$dat_process, "2023-04-25/spore_dat.rds"))
# df_offset <- read_rds(str_c(.path$dat_process, "2023-04-25/offset.rds"))
# 
# # select station-year combination with measurements >= 10, years >= 3
# df_siteyear <- df_spore %>%
#   filter(family == "Total") %>%
#   mutate(count = abs(count)) %>%
#   filter(count > 0) %>%
#   mutate(year = format(date, "%Y") %>% as.integer()) %>%
#   group_by(city, id, year) %>%
#   mutate(nobservation = n()) %>%
#   filter(nobservation >= 10) %>%
#   ungroup() %>%
#   group_by(city, id) %>%
#   mutate(nyear = length(unique(year))) %>%
#   filter(nyear >= 3) %>%
#   ungroup() %>%
#   dplyr::select(lat, lon, station, city, state, country, id, year, date, count)
# 
# # fill the dates to full years
# df_fill <- df_siteyear %>%
#   group_by(lat, lon, station, city, state, country, id) %>%
#   complete(date = seq(min(year) %>% paste0("-01-01") %>% ymd(), max(year) %>% paste0("-12-31") %>% ymd(), by = "day")) %>%
#   mutate(year = format(date, "%Y") %>% as.integer()) %>%
#   mutate(doy = format(date, "%j") %>% as.integer()) %>%
#   ungroup() %>%
#   filter(doy <= 365) %>%
#   left_join(df_siteyear %>%
#               distinct(id, .keep_all = T) %>%
#               mutate(n = row_number()) %>%
#               dplyr::select(-count, -date, -year),
#             by = c("lat", "lon", "station", "city", "state", "country", "id")) %>%
#   dplyr::select(lat, lon, station, city, state, country, id, n, year, doy, date, count)
# 
# # whittaker smoothing
# # Function for smoothing, because ptw::whit1 does not take NA in the time series
# whitfun <- function(x, lambda) {
#   max_id <- 0
#   done <- F
#   while (!done) {
#     min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
#     if (min_id == Inf) { # all numbers are NA
#       done <- T # consider this ts done
#     } else {
#       max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
#       if (max_id == Inf) {
#         max_id <- length(x) # last non-NA segment is at the end of the whole ts
#         done <- T # consider this ts done
#       }
#       x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
#     }
#   }
#   return(x)
# }
# 
# df_smooth <- df_fill %>% 
#   group_by(lat, lon, station, city, state, country, id, n) %>% 
#   mutate(count_fill = zoo::na.approx(count, maxgap = 7, na.rm = F)) %>% 
#   mutate(count_whit = whitfun(count_fill, lambda = 1800)) %>%
#   ungroup()
# 
# # determine the offset
# i = 60
# l = 100
# 
# df_median <- df_fill %>% 
#   group_by(lat, lon, station, city, state, country, id, n, doy) %>% 
#   summarise(count_md = median(count, na.rm = T)) %>% 
#   mutate(count_md = ifelse(is.nan(count_md), NA, count_md)) %>% 
#   ungroup() %>% 
#   filter(n == i) %>% 
#   #group_by(lat, lon, station, city, state, country, id, n) %>% 
#   mutate(count_whit = whitfun(count_md, lambda = l)) %>% 
#   mutate(count_fill = zoo::na.approx(count_md, maxgap = 7, na.rm = F)) %>% 
#   mutate(count_whit_fill = whitfun(count_fill, lambda = l))
# 
# offset = df_median %>% 
#   #filter(n == 3) %>% 
#   filter(count_whit_fill == min(count_whit_fill, na.rm = T)) %>% 
#   summarise(offset = head(doy, 1))
# 
# ggplot(data = df_median, aes(x = doy)) +
#   geom_point(data = df_fill %>% filter(n == i), aes(x = doy, y = count, col = year, alpha = 0.1, group = year)) +
#   guides(alpha = "none") +
#   geom_point(aes(y = count_md), col = "purple") +
#   #geom_line(aes(y = count_whit), col = "blue") +
#   geom_line(aes(y = count_whit_fill), col = "red") +
#   geom_vline(xintercept = offset$offset[1], col = "blue") +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   ylab("count") +
#   #labs(color = "lambda = 100\noffset = 52\nyear") +
#   facet_wrap(. ~ n, ncol = 6, scales = "free_y")
# 
# ggplot(data = df_smooth %>% filter(n == i)) +
#   geom_point(aes(x = date, y = count), col = "gray") +
#   geom_vline(xintercept = (df_smooth %>% filter(n == i, doy == offset$offset[1]))$date, col = "blue") +
#   geom_line(aes(x = date, y = count_whit)) +
#   scale_y_continuous(
#     trans = scales::log_trans(),
#     breaks = scales::trans_breaks("log", function(x) exp(x)),
#     labels = scales::trans_format("log", scales::math_format(e^.x))
#   ) +
#   ylab("count") +
#   scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
#   facet_wrap(. ~ n, ncol = 6, scales = "free_y")
# 
# # df_offset <- data.frame(n = 1:60) %>% 
# #   mutate(lambda = NA, offset = NA)
# df_offset[i, "lambda"] = l
# df_offset[i, "offset"] = offset$offset[1]
# 
# df_fill_smooth_offset <- left_join(df_smooth, df_offset, by = "n")
# write_rds(df_fill_smooth_offset, str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))


# visualization
df_fill_smooth_offset <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_fill_smooth_offset <- df_fill_smooth_offset %>%
  group_by(lat, lon, station, city, state, country, id, n) %>%
  mutate(count_fill = zoo::na.approx(count, maxgap = 14, na.rm = F)) %>%
  mutate(count_whit = whitfun(count_fill, lambda = 1800)) %>%
  ungroup()
write_rds(df_fill_smooth_offset, str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_plot <- tibble(
  n = 1:60,
  p = vector("list", 60),
  b = vector("list", 60)
)

for (i in c(1:60)) {
  df <- df_fill_smooth_offset %>% 
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
    geom_vline(xintercept = (df %>% filter(doy == offset))$date, col = "blue") +
    geom_line(aes(x = date, y = count_whit)) +
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
}

write_rds(df_plot, str_c(.path$dat_process, "2023-04-25/fill_smooth_offset_plot.rds"))

# save plots to pdf file
pdf(
  "output/figures/p_offset_a.pdf",
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
  "output/figures/p_offset_b.pdf",
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
  "output/figures/p_offset_c.pdf",
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