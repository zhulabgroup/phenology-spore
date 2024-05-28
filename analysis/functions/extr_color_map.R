extr_color_map <- function(df_in, p_map) {
  df <- ggplot_build(p_map)$data[[3]][c("colour", "x", "y")] %>% 
    distinct(x, .keep_all = T) %>% 
    rename(
      col = colour,
      lon = x,
      lat = y)  %>% 
    right_join(
      df_in %>% dplyr::select(-col),
      by = c("lon", "lat"))
  
  return(df)
}