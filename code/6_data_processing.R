df <- read_rds(str_c(.path$dat_process, "spore_dat.rds"))

# station_year combination, measurements > 18, years > 5
df_siteyear <- df %>%
  filter(family == "Total") %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  group_by(location, id, year) %>%
  # summarise(nobservation=n()) %>%
  mutate(nobservation = n()) %>%
  filter(nobservation >= 18) %>%
  ungroup() %>%
  group_by(location, id) %>%
  # summarise(nyear=length(unique(year))) %>%
  mutate(nyear = length(unique(year))) %>%
  filter(nyear >= 5) %>%
  # summarise(nyear=length(unique(year))) %>%
  ungroup()

# linear interpolation
df_fill <- df_siteyear %>%
  dplyr::select(lat, lon, station, location, id, year, date, count) %>%
  group_by(lat, lon, station, location, id) %>%
  padr::pad(start_val = as.Date("2007-01-01"), end_val = as.Date("2021-12-31")) %>%
  mutate(year = format(date, "%Y") %>% as.integer(), count_fill = zoo::na.approx(count, maxgap = 7, na.rm = F)) %>%
  ungroup()
#mutate(count = case_when(count >= 5 ~ count)) # set low values to NA

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
  group_by(location, id) %>%
  mutate(count_smooth = whitfun(count_fill, lambda = 1800)) %>%
  ungroup() %>%
  group_by(location, id, year) %>%
  mutate(doy = format(date, "%j") %>% as.integer(), month = format(date, "%b")) %>%
  ungroup()
write_rds(df_smooth, str_c(.path$dat_process, "fill_smooth_to2021.rds"))

# time series of pollen concentration in 29 stations after filling and smoothing
p_ts_fill_smooth <- ggplot(data = df_smooth) +
  geom_line(aes(x = date, y = count_smooth)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ggtitle("Total Spore Counts (all counts are per cubic meter of air)") +
  ylab("count") +
  facet_wrap(. ~ location, ncol = 6, scales = "free_y")
ggsave(
  plot = p_ts_fill_smooth,
  filename = "~/spore_phenology/output/figures/p_ts_fill_smooth_29stations.pdf",
  width = 18,
  height = 10
)

# determine time window
# histogram
p_samp_window <- ggplot(data = df_smooth %>%
  filter(!is.na(count_smooth))) +
  geom_histogram(aes(x = doy))
# ggplot(data = df_smooth %>%
#          filter(!is.na(count_smooth)),
#        aes(x = interaction(id, year), y = doy, group = interaction(id, year), col = as.factor(id))
#        )+
#   geom_point(alpha = 0.2)
# ggplot(data = df_smooth %>%
#          group_by(location, id, year) %>%
#          filter(!is.na(count_smooth)),
#        aes(x = doy, y = log(count_smooth+1), group = interaction(id, year))
#        )+
#   geom_path(alpha = 0.2)

# data availability in whole year vs. in time window
##whole year
p_data_avail_wholeyear <- ggplot(
  data = df_smooth %>% 
    group_by(location, id, year) %>% 
    filter(!is.na(count_smooth)) %>% 
    summarise(observ_percent = n() / 366),
  aes(x = year, y = observ_percent)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  facet_wrap(. ~ location, ncol = 6) +
  ggpubr::stat_cor(method = "pearson", label.y = 500)
ggsave(
  plot = p_data_avail_wholeyear,
  filename = "~/spore_phenology/output/figures/p_data_avail_wholeyear_29stations.pdf",
  width = 18,
  height = 10
)
##time window (Apr-Oct)
p_data_avail_timewindow <- ggplot(
  data = df_smooth %>%
    group_by(location, id, year) %>%
    filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
    filter(!is.na(count_smooth)) %>%
    summarise(observ_percent = n() / 214),
  aes(x = year, y = observ_percent)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  facet_wrap(. ~ location, ncol = 6) +
  ggpubr::stat_cor(method = "pearson", label.y = 500)
ggsave(
  plot = p_data_avail_timewindow,
  filename = "~/spore_phenology/output/figures/p_data_avail_timewindow_29stations.pdf",
  width = 18,
  height = 10
)
