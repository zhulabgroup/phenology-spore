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

tidy_smoothweiwhit_station <- function(df_raw, n_gap, lambda, column_name, new_column_name) {
  df <- df_raw %>%
    group_by(lat, lon, station, city, state, country, id, n) %>%
    mutate({{new_column_name}} := util_fill_whit(x = {{column_name}}, maxgap = n_gap, lambda = lambda, minseg = 2)) %>%
    ungroup()
  
  return(df)
}