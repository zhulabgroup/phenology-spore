# confirm the ecoregion of each station

summ_ecoregion_station <- function(df) {
  coordinates(df) <- ~lon+lat
  regions <- readOGR(paste0(.path$dat_process, "2023-04-25/na_cec_eco_l1/NA_CEC_Eco_Level1.shp"))
  crs(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  df <- spTransform(df, crs(regions))
  df$region <- over(df, regions)$NA_L1NAME
  df <- as.data.frame(df) %>% 
    rename(ecoregion = region)
  
  return(df)
}