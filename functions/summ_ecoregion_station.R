# confirm the ecoregion of each station

summ_ecoregion_station <- function(df, boundary_path) {
  # Read in the boundary shapefile using sf
  shp_path <- list.files(path = boundary_path, pattern = "\\.shp$", full.names = TRUE)
  regions <- sf::st_read(shp_path)
  
  # Create a simple features (sf) object for your points (df)
  # Assuming df contains lon and lat columns
  df_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Transform the CRS of df to match the CRS of the regions shapefile
  df_sf <- sf::st_transform(df_sf, sf::st_crs(regions))
  
  # Perform the spatial join using st_join (equivalent to 'over' in sp)
  df_sf <- sf::st_join(df_sf, regions)
  
  # Rename the resulting region column
  df_sf <- df_sf %>% 
    rename(ecoregion = NA_L1NAME)
  
  # Convert back to a data frame if needed
  df_sf <- as.data.frame(df_sf)

  return(df_sf)
}