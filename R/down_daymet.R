# download climate data from daymet dataset
#' @export
down_daymet <- function(df_meta, path) {
  if (!file.exists(path)) {
    path_temp <- tempfile(pattern = "meta_for_daymet_", fileext = ".csv")

    write.table(
      df_meta %>%
        filter(n != 54) %>% # b/c the coordinates of 54 is out of scope of daymet_batch function
        select(city, lat, lon),
      path_temp,
      sep = ",",
      col.names = T,
      row.names = F,
      quote = F
    )

    df_55 <- daymetr::download_daymet_batch(
      file_location = path_temp,
      start = 2003,
      end = 2022,
      internal = TRUE,
      simplify = TRUE,
      silent = T
    )

    df_54 <- daymetr::download_daymet(
      site = "Coeur d'Alene",
      lat = 47.72480,
      lon = -116.78900,
      start = 2003,
      end = 2022,
      internal = TRUE,
      simplify = TRUE
    )

    df_daymet_all <- rbind(df_54, df_55) %>%
      filter(measurement %in% c("prcp..mm.day.", "tmax..deg.c.", "tmin..deg.c.")) %>%
      select(-tile)

    write_rds(df_daymet_all, path)
  } else {
    df_daymet_all <- read_rds(path)
  }

  return(df_daymet_all)
}

# function to tidy the daymet data
tidyweather <- function(x) {
  # pull out the data frame
  # then pull daymet data
  datadf <- pluck(x, "data")
  datadf$site <- x[[1]]
  datadf$title <- x[[2]]
  datadf$latitude <- x[[3]]
  datadf$longitude <- x[[4]]
  datadf$altitude <- x[[5]]
  datadf$tile1 <- x[[6]]
  return(datadf)
}
