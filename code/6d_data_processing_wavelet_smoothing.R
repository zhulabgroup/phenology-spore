df_spore <- read_rds(str_c(.path$dat_process, "2023-04-25/spore_dat.rds"))

# select station-year combination with measurements >= 10, years >= 3
df_siteyear <- df_spore %>%
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




## interpolation & whittaker
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

# whittaker smoothing
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

df_fill_whit <- df_fill %>%
  filter(n == 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>%
  mutate(count_fill = zoo::na.approx(count, maxgap = 14, na.rm = F)) %>%
  mutate(count_fill_whit = whitfun(count_fill, lambda = 1800)) %>%
  ungroup()




## weighter whittaker
pacman::p_load(imputeTS)

util_whit <- function(x, lambda, w, minseg = 2) {
  max_id <- 0
  done <- F
  while (!done) {
    v_non_na <- which(!is.na(x[(max_id + 1):length(x)])) # non-NA segment
    if (length(v_non_na) == 0) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      min_id <- min(v_non_na) + (max_id) # first number that is not NA
      v_na <- which(is.na(x[min_id:length(x)])) # NA segment
      if (length(v_na) == 0) { # no more NA
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      } else {
        max_id <- min(v_na) - 1 + (min_id - 1) # index of last number in this NA segment
      }
      if (max_id - min_id + 1 < minseg) { # this non-NA segment is too short
        x[min_id:max_id] <- -9999
      } else {
        x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id]) # whitman smoothing for this non-NA segment
      }
    }
  }
  x[x == -9999] <- NA
  return(x)
}

util_fill_whit <- function(x, maxgap = Inf, lambda, minseg = 2) {
  x_fill <- imputeTS::na_replace(x, fill = -9999, maxgap = maxgap) # fill short gaps with -9999 placeholder
  w <- (x_fill != -9999) # weight = 0 at long gaps, weight = 1 at short gaps
  x_sm <- util_whit(x = x_fill, lambda = lambda, w = w, minseg = minseg)
  
  return(x_sm)
}

df_wei_whit <- df_fill %>%
  filter(n == 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>%
  mutate(count_wei_whit = util_fill_whit(x = count, maxgap = 14, lambda = 1800, minseg = 2)) %>%
  ungroup()





## wavelet smoothing
pacman::p_load(rwavelet)
pacman::p_load(glmnet)

# Return an array whose columns contain the wavelet
# basis vectors on R^n, where n is a power of 2.
get_basis = function(qmf, n) {
  W = array(0, c(n, n))
  for (j in 1:n) {
    Y = array(0, n)
    Y[j] = 1
    W[,j] = IWT_PO(Y, 0, qmf)
  }
  return(W)
}

# Denoise the observed signal y, which must align
# with a gridded domain, but may contain missing
# values (NA).  The factor wf modifies the smoothing
# parameter, e.g. wf=0.5 uses a smoothing parameter
# that is half as large as that selected by cross
# validation.
denoise = function(y, qmf, wf=0.5) {
  
  # Generate a basis on the appropriate domain.
  n = length(y)
  m = as.integer(ceiling(log(n) / log(2)))
  m = 2^m
  W = get_basis(qmf, m)
  
  # Pad y to align with W.
  y = c(y, rep(NA, m - n))
  
  # Fit to the observed data, use CV to
  # select a preliminary penalty weight.
  ii = is.finite(y)
  r = cv.glmnet(W[ii,], y[ii])
  
  # Refit with the final penalty weight.
  la = r$lambda.min * wf
  r2 = glmnet(W[ii,], y[ii], lambda=la)
  return(predict(r2, W)[1:n])
}

qmf = MakeONFilter('Symmlet', 6)

df_wavelet <- df_fill %>% 
  filter(n == 1) %>% 
  mutate(count_NA = is.finite(count)) %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>%
  mutate(count_wavelet = denoise(count, qmf)) %>% 
  ungroup()

ggplot() +
  geom_line(data = df_fill %>% filter(n == 1), aes(x = date, y = count), col = "gray") +
  geom_line(data = df_fill_whit %>% filter(n == 1), aes(x = date, y = count_fill_whit), col = "black", alpha = 0.5) +
  geom_line(data = df_wei_whit %>% filter(n == 1), aes(x = date, y = count_wei_whit), col = "red", alpha = 0.5) +
  geom_line(data = df_wavelet %>% filter(n == 1), aes(x = date, y = count_wavelet), col = "blue", alpha = 0.5) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ylab("count") +
  scale_x_datetime(breaks = "1 year", date_labels = "%Y") +
  facet_wrap(. ~ interaction(n, city, state, sep = ", "), ncol = 6, scales = "free_y")
















## visualization for each method
df_fill_smooth_offset <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_plot <- tibble(
  n = 1:60,
  a = vector("list", 60)
)

for (i in c(1:60)) {
  df_fill_whit <- df_fill_smooth_offset %>% 
    filter(n == i)
  
  df <- df_fill_whit %>% 
    mutate(count_wei_whit = util_fill_whit(x = count, maxgap = 14, lambda = 1800, minseg = 2)) %>% 
    mutate(count_NA = is.finite(count)) %>% 
    mutate(count_wavelet = denoise(count, qmf))
  
  offset <- df %>% 
    head(1) %>% 
    pull(offset)
  
  city <- df %>% 
    head(1) %>% 
    pull(city)
  
  state <- df %>% 
    head(1) %>% 
    pull(state)
  
  p_a <- ggplot(data = df) +
    geom_line(aes(x = date, y = count), col = "gray") +
    geom_line(aes(x = date, y = count_whit), col = "black", alpha = 0.5) +
    geom_line(aes(x = date, y = count_wei_whit), col = "red", alpha = 0.5) +
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
  
  df_plot$a[[i]] <- p_a
}

pdf(
  "output/figures/p_compare_smoothing.pdf",
  width = 8,
  height = 8*.618
)
for (i in c(1:60)) {
  print(df_plot$a[[i]])
}
dev.off()
