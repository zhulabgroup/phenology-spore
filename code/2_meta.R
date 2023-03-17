meta_df<-nab_df %>% 
  filter(taxa=="Total Spore Count") %>% 
  group_by(taxa, station, location, lat, lon, id) %>% 
  drop_na(count) %>% 
  filter(count>0) %>% 
  summarise(mindate=min(Date),
            maxdate=max(Date),
            n=n()) %>% 
  mutate(range=maxdate-mindate) %>%
  ungroup() %>% 
  arrange(desc(n))

site_list<-c("Houston, TX","St. Louis, MO", "San Diego, CA", "New Orleans, LA", "Philadelphia, PA", "Bellevue, NE")


p_map<-ggplot()+
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  theme_void() +
  # coord_map(projection = "albers", parameters = c(30,40), orientation = c(90,0,-105)) +
  geom_point(data=meta_df%>% filter( !location %in% site_list),aes(x=lon, y=lat), pch=1, cex=3)+
  geom_point(data=meta_df %>% filter(location %in% site_list),aes(x=lon, y=lat), pch=10, col="dark green", cex=3)+
  geom_label_repel(data=meta_df  %>% filter(location %in% site_list),aes(x=lon, y=lat, label=location), col="dark green")+
  scale_color_viridis_c()+
  theme_void()+
  coord_map("bonne", lat0 = 50)
