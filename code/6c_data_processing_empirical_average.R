df <- read_rds(str_c(.path$dat_process, "2023-04-25/spore_dat.rds"))

# select station-year combination with measurements >= 10, years >= 3
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

# fill the dates to full years
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

# determine the offset
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

df_average <- df_fill %>% 
  group_by(lat, lon, station, city, state, country, id, n, doy) %>% 
  summarise(count_avg = mean(count, na.rm = T)) %>% 
  mutate(count_avg = ifelse(is.nan(count_avg), NA, count_avg)) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  mutate(count_whit = whitfun(count_avg, lambda = 1800)) %>% 
  mutate(count_fill = zoo::na.approx(count_avg, maxgap = 7, na.rm = F)) %>% 
  mutate(count_whit_fill = whitfun(count_fill, lambda = 1800))

# visualization
ggplot(data = df_average %>% filter(n == 1), aes(x = doy)) +
  geom_point(data = df_fill %>% filter(n == 1), aes(x = doy, y = count, col = year, alpha = 0.3, group = year)) +
  geom_point(aes(y = count_avg), col = "purple") +
  geom_line(aes(y = count_whit), col = "blue") +
  geom_line(aes(y = count_whit_fill), col = "red") +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ylab("count") +
  facet_wrap(. ~ n, ncol = 6, scales = "free_y")

ggplot(data = df_smooth %>% filter(n == 1)) +
  geom_point(aes(x = date, y = count), col = "gray") +
  geom_line(aes(x = date, y = count_whit)) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ylab("count") +
  facet_wrap(. ~ id, ncol = 6, scales = "free_y")
