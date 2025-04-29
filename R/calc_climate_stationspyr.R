# calculate the TAP, MAT
#' @export
calc_climate_stationspyr <- function(df_daymet_raw, df_spore) {
  df <- df_daymet_raw %>%
    filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")) %>%
    spread(key = "measurement", value = "value") %>%
    rename(
      prcp = `prcp..mm.day.`,
      tmax = `tmax..deg.c.`,
      tmin = `tmin..deg.c.`,
      city = site,
      lat = latitude,
      lon = longitude,
      doy = yday
    ) %>%
    mutate(date = as.Date(doy, origin = paste0(year, "-01-01")) - 1) %>%
    mutate(temp = (tmax + tmin / 2)) %>%
    select(city, lat, lon, year, doy, date, temp, prcp) %>%
    right_join(df_spore, by = c("lat", "lon", "city", "year", "doy", "date")) %>%
    group_by(lat, lon, station, city, state, country, id, n, offset, year_new) %>%
    summarise(
      mat = mean(temp, na.rm = T),
      tap = sum(prcp, na.rm = T)
    )

  return(df)
}
