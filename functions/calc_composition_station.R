# 55 stations included in our analysis
df_station <- df_spore %>%
  left_join(
    df_full %>%
      distinct(n, .keep_all = T) %>%
      dplyr::select(station, state, n),
    by = c("station", "state")
  ) %>%
  drop_na(n)

# community composition in each year across all stations
p_comm_pie <- df_station %>%
  filter(family != "Total" | is.na(family)) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  group_by(year) %>%
  summarise(total = sum(count, na.rm = T)) %>%
  ungroup() %>%
  right_join(
    df_station %>% mutate(year = format(date, "%Y") %>% as.integer()),
    by = c("year")
  ) %>%
  mutate(family = ifelse(is.na(family), "Unidentified", family)) %>%
  group_by(year, total, family) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  mutate(pctg = count / total) %>%
  arrange(desc(pctg)) %>%
  ungroup() %>%
  filter(family != "Total") %>%
  group_by(family) %>%
  summarise(pctg = median(pctg)) %>%
  arrange(desc(pctg)) %>%
  ungroup() %>%
  rename("Family" = "family") %>%
  ggplot(aes(x = "", y = pctg, fill = Family)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right")
