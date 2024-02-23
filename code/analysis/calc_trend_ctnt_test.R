df <- df_ana %>%
  filter(Metric == "SOS") %>%
  filter(cpltness >= 0.8) %>%
  drop_na(Value) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>%
  filter(n() >= 5) %>%
  mutate(Nyear = max(year_new) - min(year_new) + 1) %>% 
  ungroup()

your_starting_value <- 0.1 
m_ctnt <- lmerTest::lmer(
  Value ~ year_new + (year_new|n),
  data = df,
  start = list(theta = c(1,100,0.5))
)
