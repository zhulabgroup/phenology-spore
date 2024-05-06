df_station <- df_spore %>% 
  left_join(
    df_full %>% 
      distinct(n, .keep_all = T) %>% 
      dplyr::select(station, state, n),
    by = c("station", "state")) %>% 
  drop_na(n)

df_comm <- df_station %>% 
  filter(family != "Total" | is.na(family)) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  summarise(total = sum(count, na.rm = T)) %>% 
  ungroup() %>% 
  right_join(
    df_station %>% mutate(year = format(date, "%Y") %>% as.integer()),
    by = c("lat", "lon", "station", "city", "state", "country", "id", "n")) %>% 
  mutate(family = ifelse(is.na(family), "Unidentified", family)) %>% 
  group_by(lat, lon, station, city, state, country, id, n, total, family) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  mutate(pctg = count / total) %>% 
  arrange(desc(family)) %>% 
  ungroup() %>% 
  filter(family != "Total")

p_comm <- ggplot(data = df_comm, aes(x = "", y = pctg, fill = family)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  guides(color = F) +
  facet_wrap(~ n + city)