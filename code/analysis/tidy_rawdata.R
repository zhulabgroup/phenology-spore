rawnab_df <- readRDS("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/nab_renew.rds")
station_df <- read.csv("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/renew_station_info.csv")
taxa_df <- read.csv("/nfs/turbo/seas-zhukai/phenology/nab/clean/2023-04-25/renew_taxonomy.csv")
nab_df <- rawnab_df %>% 
  rename(id = stationid) %>% 
  left_join(station_df, by = "id")
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
    taxa_raw == "Other Fungi" ~ "Other Fungi",
    taxa_raw == "Unidentified Fungi" ~ "Unidentified Fungi",
    TRUE ~ family
  )) %>%
  mutate(genus = case_when(
    taxa_raw == "Total Spore Count" ~ "Total_o",
    taxa_raw == "Other Fungi" ~ "Other Fungi",
    taxa_raw == "Unidentified Fungi" ~ "Unidentified Fungi",
    TRUE ~ genus
  )) %>%
  filter(kingdom %in% c("Fungi", "Protozoa", "Chromista")) %>%
  rename(station = name) %>% 
  group_by(date, lat, lon, station, city, state, country, id, family) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Not all sites have "total spore count"
df_add <- df_o %>%
  filter(family != "Total_o" | is.na(family)) %>%
  group_by(date, lat, lon, station, city, state, country, id) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    family = "Total",
    genus = "Total"
  )

df <- bind_rows(df_o, df_add)
write_rds(df, "/nfs/turbo/seas-zhukai/phenology/phenology_spore/processed/2023-04-25/spore_dat.rds")
