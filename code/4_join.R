nab_with_taxa_df <- nab_df %>%
  rename(taxa_raw = taxa) %>%
  left_join(nab_taxa_df, by = "taxa_raw") %>%
  rename(taxa = taxa_clean) %>%
  mutate(family = case_when(
    taxa_raw == "Total Spore Count" ~ "Total",
    TRUE ~ family
  )) %>%
  mutate(genus = case_when(
    taxa_raw == "Total Spore Count" ~ "Total",
    TRUE ~ genus
  )) %>%
  filter(kingdom == "Fungi" | is.na(kingdom)) %>%
  group_by(Date, lat, lon, station,location, id, family, genus, taxa) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(date = as.Date(Date)) %>%
  dplyr::select(-Date)