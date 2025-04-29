#' @export

tidy_nab_ts <- function(df_full, path_offset, n_gap = 14, lambda = 100) {
  df_offset <- df_full %>%
    left_join(read_offset_station(path = path_offset), by = c("lat", "lon", "station", "state", "country", "id")) %>%
    rename(lmd_offset = lambda)

  df_smooth <- df_offset %>%
    tidy_smoothfillwhit_station(n_gap = n_gap, lambda = lambda, column_name = count, new_column_name = count_fillwhit)

  df_sporeyr <- df_smooth %>% tidy_sporeyr_station()

  df_cpltness <- calc_completeness_stationspyr(df_sporeyr, column_name = count_fillwhit)

  return(df_cpltness)
}

# read the offset of each station
read_offset_station <- function(path) {
  df_offset <- read_rds(path) %>%
    group_by(lat, lon, station, state, country, id, lambda, offset) %>%
    summarise(
      lat = head(lat, 1),
      lon = head(lon, 1),
      station = head(station, 1),
      state = head(state, 1),
      country = head(country, 1),
      id = head(id, 1),
      lambda = head(lambda, 1),
      offset = head(offset, 1),
    ) %>%
    ungroup() %>%
    filter(country == "US") %>%
    filter(!(state %in% c("PR", "AK", "HI")))

  return(df_offset)
}

# interpolate gaps <= n_gap
# whittaker smoothing with lambda
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

tidy_smoothfillwhit_station <- function(df_raw, n_gap, lambda, column_name, new_column_name) {
  df <- df_raw %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    mutate({{ new_column_name }} := zoo::na.approx({{ column_name }}, maxgap = n_gap, na.rm = F)) %>%
    mutate({{ new_column_name }} := whitfun({{ new_column_name }}, lambda)) %>%
    ungroup()

  return(df)
}

# define spore year for each station

tidy_sporeyr_station <- function(df_raw) {
  df <- df_raw %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    mutate(date = as.Date(date)) %>%
    mutate(date_new = case_when(
      offset < 183 ~ date - offset,
      T ~ date + 365 - offset
    )) %>%
    mutate(year_new = format(date_new, "%Y") %>% as.numeric()) %>%
    mutate(doy_new = format(date_new, "%j") %>% as.numeric()) %>%
    ungroup()

  return(df)
}

# calculate data completeness in each station-sporeyr for future quality control

calc_completeness_stationspyr <- function(df_raw, column_name) {
  df <- df_raw %>%
    group_by(lat, lon, station, city, state, country, id, n, year_new) %>%
    mutate(cpltness = sum(!is.na({{ column_name }})) / 365) %>%
    ungroup()

  return(df)
}
