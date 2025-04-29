# only include stations in continental US
# zoom in to a specific family f
# include stations with measurements >= n_m in each year
# and years >= n_yr in each station

#' @export
tidy_taxa_station_raw_data <- function(df_spore, f, n_m, n_yr, rename = T) {
  df <- df_spore %>%
    filter(family == f) %>%
    filter(country == "US") %>%
    filter(!(state %in% c("PR", "AK", "HI"))) %>%
    mutate(count = abs(count)) %>%
    filter(count > 0) %>%
    mutate(year = format(date, "%Y") %>% as.integer()) %>%
    group_by(city, id, year) %>%
    mutate(nobservation = n()) %>%
    filter(nobservation >= n_m) %>%
    ungroup() %>%
    group_by(city, id) %>%
    mutate(nyear = length(unique(year))) %>%
    filter(nyear >= n_yr) %>%
    ungroup() %>%
    select(lat, lon, station, city, state, country, id, year, date, count)

  if (rename) {
    df <- tidy_rename_number_station(df)
  }
  return(df)
}

# rename stations in the same city
# tag number for each station

#' @export
tidy_rename_number_station <- function(df_raw) {
  df <- df_raw %>%
    left_join(
      df_raw %>%
        distinct(id) %>%
        mutate(n = row_number()),
      by = "id"
    ) %>%
    mutate(city = ifelse(n == 8, "Lexington (Station 1)", city)) %>%
    mutate(city = ifelse(n == 16, "Las Vegas (Station 1)", city)) %>%
    mutate(city = ifelse(n == 52, "Las Vegas (Station 2)", city)) %>%
    mutate(city = ifelse(n == 12, "Oklahoma City (Station 1)", city)) %>%
    mutate(city = ifelse(n == 13, "Oklahoma City (Station 2)", city)) %>%
    mutate(city = ifelse(n == 49, "San Antonio", city)) %>%
    mutate(city = ifelse(n == 14, "Tulsa (Station 1)", city)) %>%
    mutate(city = ifelse(n == 1, "Tulsa (Station 2)", city)) %>%
    mutate(city = ifelse(n == 36, "Monroeville", city))

  return(df)
}
