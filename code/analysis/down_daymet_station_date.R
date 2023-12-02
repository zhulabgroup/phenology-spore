# download climate data from daymet dataset

down_daymet_station_date <- function(meta) {
  write.table(df_meta %>% 
                filter(n != 54) %>% # b/c the coordinates of 54 is out of scope of daymet_batch function
                dplyr::select(city, lat, lon),
              str_c(.path$dat_process, "2023-04-25/meta_for_daymet.csv"),
              sep = ",",
              col.names = T,
              row.names = F,
              quote = F)
  
  df_daymet_alldate <- daymetr::download_daymet_batch(file_location = str_c(.path$dat_process, "2023-04-25/meta_for_daymet.csv"),
                                              start = 2003,
                                              end = 2022,
                                              internal = TRUE,
                                              simplify = TRUE,
                                              silent = T)
  
  df_54 <- daymetr::download_daymet(
    site = "Coeur d'Alene",
    lat = 47.72480,
    lon = -116.78900,
    start = 2003,
    end = 2022,
    internal = TRUE,
    simplify = TRUE)
  
  
}

# function to tidy the daymet data
tidyweather  <- function(x) {
  # pull out the data frame
  # then pull daymet data
  datadf  <- pluck(x, "data")
  datadf$site       <- x[[1]]
  datadf$title      <- x[[2]]
  datadf$latitude   <- x[[3]]
  datadf$longitude  <- x[[4]]
  datadf$altitude   <- x[[5]]
  datadf$tile1      <- x[[6]]
  return(datadf)
}