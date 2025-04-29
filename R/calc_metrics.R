# extract ten metrics for each station-sporeyr

calc_metrics <- function(df_ts) {
  df_peak <- df_ts %>%
    drop_na(count_fillwhit) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    filter(count_fillwhit == max(count_fillwhit)) %>%
    summarise(
      peak = head(count_fillwhit, 1),
      peak_doy = head(doy_new, 1),
      peak_date_old = head(date, 1)
    ) %>%
    mutate(peak = ifelse(peak_doy %in% 11:355, peak, NA)) %>%
    right_join(df_ts, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "cpltness")) %>%
    drop_na(peak) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    filter(doy_new %in% (peak_doy - 10):(peak_doy + 10)) %>%
    mutate(peak = ifelse(sum(is.na(count_fillwhit)) != 0, NA, peak)) %>%
    summarise(
      peak = head(peak, 1),
      peak_doy = head(peak_doy, 1),
      peak_date_old = head(peak_date_old, 1)
    )

  df_amplitude <- df_ts %>%
    drop_na(count_fillwhit) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    filter(count_fillwhit == min(count_fillwhit)) %>%
    summarise(
      trough = head(count_fillwhit, 1),
      trough_doy = head(doy_new, 1),
      trough_date_old = head(date, 1)
    ) %>%
    mutate(
      trough_start = case_when(
        trough_doy <= 10 ~ 1,
        TRUE ~ trough_doy - 10
      )
    ) %>%
    mutate(
      trough_end = case_when(
        year_new %% 4 != 0 & trough_doy >= 356 ~ 365,
        year_new %% 4 == 0 & trough_doy >= 357 ~ 366,
        TRUE ~ trough_doy + 10
      )
    ) %>%
    right_join(df_ts, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "cpltness")) %>%
    drop_na(trough) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    filter(doy_new %in% trough_start:trough_end) %>%
    mutate(trough = ifelse(sum(is.na(count_fillwhit)) != 0, NA, trough)) %>%
    summarise(
      trough = head(trough, 1),
      trough_doy = head(trough_doy, 1),
      trough_date_old = head(trough_date_old, 1)
    ) %>%
    full_join(df_peak, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "cpltness")) %>%
    mutate(amplitude = peak - trough)

  df_integral <- df_ts %>%
    drop_na(count_fillwhit) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    summarise(integral = sum(count_fillwhit) / n() * 365)

  df_season <- df_ts %>%
    drop_na(count_fillwhit) %>%
    filter(cpltness >= 1) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    filter(cumsum(count_fillwhit) >= 0.1 * sum(count_fillwhit) & cumsum(count_fillwhit) <= 0.9 * sum(count_fillwhit)) %>%
    summarise(
      sos = min(doy_new),
      eos = max(doy_new),
      sos_date_old = min(date),
      eos_date_old = max(date)
    ) %>%
    mutate(los = eos - sos)

  df_allergy_season <- df_ts %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    group_modify(~ find_sas(.)) %>%
    ungroup() %>%
    rename(
      sas = out_doy,
      sas_date_old = out_date_old
    ) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    arrange(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, desc(doy_new)) %>%
    group_modify(~ find_sas(.)) %>%
    arrange(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, doy_new) %>%
    ungroup() %>%
    rename(
      eas = out_doy,
      eas_date_old = out_date_old
    ) %>%
    mutate(las = eas - sas + 1) %>%
    drop_na(count_fillwhit) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    summarise(
      sas = head(sas, 1),
      eas = head(eas, 1),
      las = head(las, 1),
      sas_date_old = head(sas_date_old, 1),
      eas_date_old = head(eas_date_old, 1)
    )

  df_integral_as <- df_allergy_season %>%
    right_join(df_ts, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "cpltness")) %>%
    drop_na(count_fillwhit) %>%
    drop_na(las) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, sas, eas, las, sas_date_old, eas_date_old) %>%
    filter(doy_new %in% sas:eas) %>%
    summarise(
      cpltness_as = n() / las,
      integral_as = sum(count_fillwhit) / n() * las
    ) %>%
    ungroup() %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness) %>%
    summarise(
      cpltness_as = head(cpltness, 1),
      integral_as = head(integral_as, 1)
    )

  df <- list(
    df_amplitude,
    df_integral,
    df_season,
    df_allergy_season,
    df_integral_as
  ) %>%
    reduce(full_join, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new", "cpltness")) %>%
    mutate(ln_peak = log(peak + 1)) %>%
    mutate(ln_integral = log(integral + 1)) %>%
    mutate(ln_integral_as = log(integral_as + 1)) %>%
    mutate(ln_amplitude = log(amplitude + 1))

  # Gather df_metrics into a long df
  df <- df %>% tidy_gathermetrics()

  # Assign color palette for 55 stations
  color_palette <- colorRampPalette(colors = rainbow(55))
  df <- df %>% mutate(col = color_palette(55)[n])

  return(df)
}

find_sas <- function(x) {
  # find the first day of 10 consecutive days with spore count >= threshold (thrsh)
  # 10 values before sas are not NA, otherwise, return NA
  # if sas <= 10, all the values before sas should not be NA, otherwise return NA
  # reverse order for eas
  consecutive_count <- 0
  out_doy <- NA
  out_date_old <- NA
  for (i in 1:nrow(x)) {
    if (!is.na(x$count_fillwhit[i]) && x$count_fillwhit[i] >= 4605) {
      consecutive_count <- consecutive_count + 1
      if (consecutive_count == 1) {
        out_doy <- x$doy_new[i]
        out_date_old <- x$date[i]
      }
      if (consecutive_count == 10) {
        break
      }
    } else {
      consecutive_count <- 0
    }
  }
  if (!is.na(out_doy) && i > 10 && any(is.na(x$count_fillwhit[(i - 10):i]))) {
    out_doy <- NA
    out_date_old <- NA
  } else if (!is.na(out_doy) && i <= 10 && any(is.na(x$count_fillwhit[1:i]))) {
    out_doy <- NA
    out_date_old <- NA
  }
  x$out_doy <- out_doy
  x$out_date_old <- out_date_old
  return(x)
}

# gather df_metrics into a long df and name the metrics
tidy_gathermetrics <- function(df_in) {
  df <- df_in %>%
    gather(key = "Metric", value = "Value", peak, ln_peak, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>%
    mutate(cpltness = ifelse(Metric %in% c("sas", "eas", "las"), 1, cpltness)) %>%
    mutate(cpltness = ifelse(Metric %in% c("integral_as", "ln_integral_as"), cpltness_as, cpltness)) %>%
    dplyr::select(lat, lon, station, city, state, country, id, n, offset, year_new, cpltness, Metric, Value) %>%
    mutate(Metric = ifelse(Metric == "sos", "SOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "eos", "EOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "los", "LOS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "sas", "SAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "eas", "EAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "las", "LAS", Metric)) %>%
    mutate(Metric = ifelse(Metric == "peak", "Cp", Metric)) %>%
    mutate(Metric = ifelse(Metric == "amplitude", "Ca", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral", "AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "integral_as", "ASIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_peak", "ln_Cp", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_amplitude", "ln_Ca", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral", "ln_AIn", Metric)) %>%
    mutate(Metric = ifelse(Metric == "ln_integral_as", "ln_ASIn", Metric))

  return(df)
}
