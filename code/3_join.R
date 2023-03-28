df_o <- nab_df %>%
  rename(taxa_raw = taxa) %>%
  left_join(taxa_df, by = "taxa_raw") %>%
  rename(taxa = taxa_clean) %>%
  mutate(kingdom = case_when(
    taxa_raw == "Total Spore Count" ~ "Fungi",
    TRUE ~ kingdom
  )) %>%
  mutate(family = case_when(
    taxa_raw == "Total Spore Count" ~ "Total_o",
    TRUE ~ family
  )) %>%
  mutate(genus = case_when(
    taxa_raw == "Total Spore Count" ~ "Total_o",
    TRUE ~ genus
  )) %>%
  filter(kingdom %in% c("Fungi", "Protozoa", "Chromista")) %>%
  group_by(date, lat, lon, station, location, id, family) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Not all sites have "total spore count"
df_add <- df_o %>%
  filter(family != "Total_o" | is.na(family)) %>%
  group_by(date, lat, lon, station, location, id) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    family = "Total",
    genus = "Total"
  )

left_join(
  df_o %>%
    filter(family == "Total_o") %>%
    select(date, location, total_o = count),
  df_add %>%
    select(date, location, total = count),
  by = c("date", "location")
) %>%
  ggplot() +
  geom_point(aes(x = total_o, y = total)) +
  theme_classic()

df <- bind_rows(df_o, df_add)
write_rds(df, str_c(.path$dat_process, "spore_dat.rds"))
