plot_ecoregion <- function(df_meta, boundary_path) {
  conic_projection <- sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")
  us_states <- tigris::states(class = "sf") %>% sf::st_transform(conic_projection)
  contus_states <- us_states[us_states$STUSPS %in% c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"), ]
  us_cont <- contus_states %>% sf::st_union()

  shp_path <- list.files(path = boundary_path, pattern = "\\.shp$", full.names = TRUE)
  df_base <- sf::st_read(shp_path) %>%
    sf::st_transform(conic_projection) %>%
    sf::st_simplify(dTolerance = 0.0001)
  us_cont <- sf::st_simplify(us_cont, dTolerance = 0.0001)
  us_epa <- df_base %>%
    sf::st_transform(conic_projection) %>%
    sf::st_intersection(us_cont) %>%
    rename(Ecoregions = NA_L1NAME) %>%
    mutate(col_value = case_when(
      Ecoregions == "EASTERN TEMPERATE FORESTS" ~ "#BDDB8E",
      Ecoregions == "GREAT PLAINS" ~ "#F8CD9D",
      Ecoregions == "MEDITERRANEAN CALIFORNIA" ~ "#D2E5B8",
      Ecoregions == "NORTH AMERICAN DESERTS" ~ "#F9DC6B",
      Ecoregions == "MARINE WEST COAST FOREST" ~ "#45C1BD",
      Ecoregions == "NORTHERN FORESTS" ~ "#AFDEE5",
      Ecoregions == "TROPICAL WET FORESTS" ~ "#C674AF",
      Ecoregions == "NORTHWESTERN FORESTED MOUNTAINS" ~ "#5FBC56",
      Ecoregions == "SOUTHERN SEMIARID HIGHLANDS" ~ "#DBD492",
      Ecoregions == "TEMPERATE SIERRAS" ~ "#C9E69A",
      Ecoregions == "WATER" ~ "#6B91CB"
    )) %>%
    mutate(Ecoregions = case_when(
      Ecoregions == "EASTERN TEMPERATE FORESTS" ~ "Eastern Temperate Forests",
      Ecoregions == "GREAT PLAINS" ~ "Great Plains",
      Ecoregions == "MEDITERRANEAN CALIFORNIA" ~ "Mediterranean California",
      Ecoregions == "NORTH AMERICAN DESERTS" ~ "North American Deserts",
      Ecoregions == "MARINE WEST COAST FOREST" ~ "Marine West Coast Forest",
      Ecoregions == "NORTHERN FORESTS" ~ "Northern Forests",
      Ecoregions == "TROPICAL WET FORESTS" ~ "Tropical Wet Forests",
      Ecoregions == "NORTHWESTERN FORESTED MOUNTAINS" ~ "Northwestern Forested Mountains",
      Ecoregions == "SOUTHERN SEMIARID HIGHLANDS" ~ "Southern Semiarid Highlands",
      Ecoregions == "TEMPERATE SIERRAS" ~ "Temperate Sierras",
      Ecoregions == "WATER" ~ "Water"
    ))
  col_map <- us_epa %>%
    distinct(Ecoregions, .keep_all = T) %>%
    select(Ecoregions, col_value)
  df_meta_sf <- sf::st_as_sf(df_meta, coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>%
    sf::st_transform(conic_projection)

  p <- ggplot(us_epa) +
    geom_sf(aes(group = Ecoregions, fill = Ecoregions, color = Ecoregions)) +
    scale_fill_manual(
      values = setNames(col_map$col_value, col_map$Ecoregions),
      name = "Ecoregions"
    ) +
    scale_color_manual(
      values = setNames(col_map$col_value, col_map$Ecoregions),
      name = "Ecoregions"
    ) +
    geom_sf(data = contus_states, fill = NA, col = "gray50", alpha = 0.5) +
    geom_sf(data = df_meta_sf, col = "darkgreen", pch = 2, size = 3) +
    theme_void()

  return(p)
}
