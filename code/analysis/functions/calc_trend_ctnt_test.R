df <- df_ana %>%
  filter(Metric == "SOS") %>%
  filter(cpltness >= 0.8) %>%
  drop_na(Value) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>%
  filter(n() >= 5) %>%
  mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
  ungroup()

m_ctnt <- lme(
  Value ~ year_new,
  data = df,
  random = ~ year_new | n)

summary(m_ctnt)
