meta_df <- df %>%
  group_by(station, location, lat, lon, id) %>%
  drop_na(count) %>%
  filter(count > 0) %>%
  summarise(
    mindate = min(date),
    maxdate = max(date),
    n = date %>% unique() %>% length()
  ) %>%
  mutate(range = maxdate - mindate) %>%
  ungroup() %>%
  arrange(desc(n))

site_list <- c("Houston, TX", "St. Louis, MO", "San Diego, CA", "New Orleans, LA", "Philadelphia, PA", "Bellevue, NE")
site_list_order<- meta_df %>% filter(location %in% site_list) %>% arrange(lat) %>% pull(location)
  
p_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  theme_void() +
  geom_point(data = meta_df %>% filter(!location %in% site_list), aes(x = lon, y = lat), pch = 1, cex = 3) +
  geom_point(data = meta_df %>% filter(location %in% site_list), aes(x = lon, y = lat), pch = 10, col = "dark green", cex = 3) +
  geom_label_repel(data = meta_df %>% filter(location %in% site_list), aes(x = lon, y = lat, label = location), col = "dark green") +
  scale_color_viridis_c() +
  theme_void() +
  # coord_map(projection = "albers", parameters = c(30,40), orientation = c(90,0,-105)) +
  coord_map("bonne", lat0 = 50)
