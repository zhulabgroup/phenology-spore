comm_meta <- df %>%
  filter(family != "Total_o" & family != "Total") %>%
  group_by(location, family) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  group_by(location) %>%
  mutate(dom = count / sum(count, na.rm = T)) %>%
  ungroup() %>%
  group_by(family) %>%
  summarise(dom = median(dom, na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(dom))
head(comm_meta)

df_comm_major <- df %>%
  filter(family %in% (comm_meta %>% slice(1:5) %>% pull(family)) |
    family == "Total") %>%
  select(location, date, family, count) %>%
  group_by(location, family) %>%
  tidyr::complete(date = seq(min(date), max(date), by = "1 day")) %>%
  ungroup() %>%
  mutate(count = case_when(count >= 5 ~ count)) # set low values to 0

p_comm <- ggplot(data = df_comm_major %>%
  filter(location %in% site_list) %>%
  filter(lubridate::year(date) == 2012) %>%
  drop_na()) +
  geom_line(aes(x = date, y = count, col = family, group = family), alpha = 0.5) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  ggtitle("Taxa-specific Spore Counts (all counts are per cubic meter of air)") +
  ylab("log (count)") +
  theme_classic() +
  facet_wrap(. ~ location, ncol = 2, scales = "free_y") +
  theme(legend.position = "bottom")
