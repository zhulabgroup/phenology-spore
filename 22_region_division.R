df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))

df_stations <- df_smooth %>% 
  distinct(n, .keep_all = T) %>% 
  filter(country == "US") %>% 
  filter(state != "PR") %>% 
  filter(state != "AK")
coordinates(df_stations) <- ~lon+lat
regions <- readOGR(paste0(.path$dat_process, "2023-04-25/na_cec_eco_l1/NA_CEC_Eco_Level1.shp"))
crs(df_stations) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
df_stations <- spTransform(df_stations, crs(regions))
df_stations$region <- over(df_stations, regions)$NA_L1NAME
df_stations <- as.data.frame(df_stations)