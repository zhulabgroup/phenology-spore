taxa_df <- read_rds(str_c(.path$nab_clean, "nab_dat.rds")) %>%
  distinct(taxa) %>%
  filter(!taxa %in% c("Total Pollen Count", "Total Spore Count")) %>%
  rename(taxa_raw = taxa) %>%
  rowwise() %>%
  mutate(taxa_clean = str_split(taxa_raw, pattern = "/", simplify = T)[1]) %>%
  mutate(taxa_clean = str_split(taxa_clean, pattern = " \\(", simplify = T)[1]) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "-type", "")) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Unidentified ", "")) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Undifferentiated ", "")) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Other ", "")) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "spores", "mycota")) %>%
  mutate(taxa_clean = str_replace(taxa_clean, pattern = " ", "")) %>%
  mutate(taxa_clean = case_when(
    taxa_clean == "Rusts" ~ "Pucciniales",
    taxa_clean == "Smuts" ~ "Myxomycetes",
    taxa_clean == "Dreshslera" ~ "Helminthosporium",
    (taxa_clean == "GrassPollen" | taxa_clean == "WeedPollen") ~ "Gramineae",
    taxa_clean == "TreePollen" ~ "Tracheophyta",
    taxa_clean == "Pollen" ~ "Viridiplantae",
    TRUE ~ taxa_clean
  )) %>%
  arrange(taxa_clean)

resolve_df <- taxa_df %>%
  pull(taxa_clean) %>%
  gnr_resolve(data_source_ids = c(4, 11), with_context = T, best_match_only = T, fields = "all") %>% # NCBI and GBIF
  full_join(taxa_df,
    by = c("user_supplied_name" = "taxa_clean")
  ) %>%
  rename(taxa_clean = user_supplied_name) %>%
  mutate(same = (taxa_clean == matched_name)) %>%
  dplyr::select(taxa_clean, classification_path, data_source_id, taxon_id)

resolve_df_correct <- resolve_df %>%
  filter(str_detect(classification_path, "Metazoa")) %>%
  pull(taxa_clean) %>%
  gnr_resolve(data_source_ids = c(4, 11), best_match_only = F, fields = "all") %>% # NCBI and GBIF
  filter(!str_detect(classification_path, "Animalia")) %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  rename(taxa_clean = user_supplied_name) %>%
  group_by(taxa_clean) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(taxa_clean, classification_path, data_source_id, taxon_id)

resolve_df <- resolve_df %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  bind_rows(resolve_df_correct) %>%
  arrange(taxa_clean)

taxa_id_df <- resolve_df %>%
  dplyr::select(taxa_clean, data_source_id, taxon_id) %>%
  distinct(taxa_clean, .keep_all = T) %>%
  mutate(data_source = case_when(
    data_source_id == 4 ~ "ncbi",
    data_source_id == 11 ~ "gbif"
  ))

taxa_class_df <- vector(mode = "list", length = nrow(taxa_id_df))
for (i in 1:nrow(taxa_id_df)) {
  taxa_class_df[[i]] <-
    classification(taxa_id_df$taxon_id[i], db = taxa_id_df$data_source[i])[[1]] %>%
    as_tibble() %>%
    filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species")) %>%
    mutate(rank = factor(rank, levels = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))) %>%
    dplyr::select(-id) %>%
    spread(key = "rank", value = "name") %>%
    mutate(taxa_clean = taxa_id_df$taxa_clean[i])
}
taxa_class_df <- bind_rows(taxa_class_df)

taxa_df <- taxa_df %>%
  left_join(taxa_class_df, by = "taxa_clean") %>%
  mutate(kingdom = str_replace(kingdom, "Plantae", "Viridiplantae")) %>%
  mutate(kingdom = case_when(
    phylum == "Oomycota" ~ "Chromista",
    TRUE ~ kingdom
  )) %>%
  as_tibble()
write_rds(taxa_df, str_c(.path$nab_clean, "nab_taxa.rds"))
