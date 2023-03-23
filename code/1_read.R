file_list <- list.files(path = str_c(.path$nab_raw, "2021-10-04/"), pattern = ".xlsx", full.names = T)

station_df <- read_csv(str_c(.path$nab_raw, "NAB stations.csv")) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

df_list <- vector(mode = "list", length = length(file_list))
for (i in 1:length(file_list)) {
  file <- file_list[i]

  dat <- read_excel(
    file,
    col_names = F
  )
  start_n <- which(dat[, 1] == "Date")

  if (min(start_n) == 1) {
    meta_dat <- NA
    station <- NA
  } else {
    meta_dat <- read_excel(
      file,
      col_names = F,
      n_max = start_n[1] - 1
    ) %>% pull()
    station <- meta_dat[2] %>%
      strsplit(split = ": ") %>%
      unlist() %>%
      tail(1)
  }

  location <- file %>%
    strsplit(split = c("/")) %>%
    unlist() %>%
    tail(1) %>%
    strsplit(split = " \\(") %>%
    unlist() %>%
    head(1)

  if (!is.na(station)) {
    station_info <- data.frame(station = station, location = location) %>%
      stringdist_inner_join(station_df, by = c("location", "station"), max_dist = 20, distance_col = "distance") %>%
      arrange(station.distance, location.distance) %>%
      head(1) %>%
      rename(station = station.y) %>%
      rename(location = location.y) %>%
      dplyr::select(-station.x, -location.x, -station.distance, -location.distance, -distance)
  } else {
    station_info <- data.frame(location = location) %>%
      stringdist_inner_join(station_df, by = "location", max_dist = 20, distance_col = "distance") %>%
      arrange(distance) %>%
      head(1) %>%
      rename(location = location.y) %>%
      dplyr::select(-location.x, -distance)
  }

  if (length(start_n) == 1) { # only pollen, no spores
    pollen_dat <- read_excel(
      file,
      skip = start_n[1] - 1,
      col_types = c(
        "date",
        rep("numeric", ncol(read_excel(
          file,
          skip = start_n[1] - 1
        )) - 1)
      )
    )
    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available)
      genus_names <- read_excel(
        file,
        skip = start_n[1] - 1
      ) %>%
        slice(1) %>%
        gather(key = "old", value = "new") %>%
        mutate(new = case_when(
          is.na(new) ~ old,
          TRUE ~ new
        ))
      pollen_dat <- read_excel(
        file,
        skip = start_n[1] - 1,
        col_types = c(
          "date",
          rep("numeric", ncol(read_excel(
            file,
            skip = start_n[1] - 1
          )) - 1)
        ),
        col_names = genus_names$new
      )
    }
    pollen_dat <- pollen_dat %>%
      filter(!is.na(Date)) %>%
      gather(key = "taxa", value = "count", -Date) %>%
      filter(!str_detect(taxa, "Station")) %>%
      filter(!taxa %in% c("Comment", "WeatherNotes")) %>%
      mutate(group = "pollen") %>%
      rowwise() %>%
      # mutate(taxa=(strsplit(taxa, split=(" \\(")) %>% unlist() %>% head(1))) %>%
      group_by(Date, taxa, group) %>%
      summarise(count = sum(count)) %>%
      ungroup()
    df_list[[i]] <- pollen_dat %>%
      cbind(station_info)
  }

  if (length(start_n) == 2) { # both pollen and spores
    pollen_dat <- read_excel(
      file,
      skip = start_n[1] - 1,
      n_max = (start_n[2] - 2) - (start_n[1] + 1),
      col_types = c(
        "date",
        rep("numeric", ncol(read_excel(
          file,
          skip = start_n[1] - 1,
          n_max = (start_n[2] - 2) - (start_n[1] + 1)
        )) - 1)
      )
    )

    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available)
      genus_names <- read_excel(
        file,
        skip = start_n[1] - 1,
        n_max = (start_n[2] - 2) - (start_n[1] + 1)
      ) %>%
        slice(1) %>%
        gather(key = "old", value = "new") %>%
        mutate(new = case_when(
          is.na(new) ~ old,
          TRUE ~ new
        ))
      pollen_dat <- read_excel(
        file,
        skip = start_n[1] - 1,
        n_max = (start_n[2] - 2) - (start_n[1] + 1),
        col_types = c(
          "date",
          rep("numeric", ncol(read_excel(
            file,
            skip = start_n[1] - 1,
            n_max = (start_n[2] - 2) - (start_n[1] + 1)
          )) - 1)
        ),
        col_names = genus_names$new
      )
    }
    pollen_dat <- pollen_dat %>%
      filter(!is.na(Date)) %>%
      gather(key = "taxa", value = "count", -Date) %>%
      filter(!taxa %in% c("Comment", "WeatherNotes", "Station Name", "Station Postal Code", "Station State")) %>%
      mutate(group = "pollen") %>%
      rowwise() %>%
      # mutate(taxa=(strsplit(taxa, split=(" \\(")) %>% unlist() %>% head(1))) %>%
      group_by(Date, taxa, group) %>%
      summarise(count = sum(count)) %>%
      ungroup()

    spore_dat <- read_excel(
      file,
      skip = start_n[2] - 1,
      col_types = c(
        "date",
        rep("numeric", ncol(read_excel(
          file,
          skip = start_n[2] - 1
        )) - 1)
      )
    ) %>%
      filter(!is.na(Date)) %>%
      gather(key = "taxa", value = "count", -Date) %>%
      filter(!str_detect(taxa, "Station")) %>%
      filter(!taxa %in% c("Comment", "WeatherNotes")) %>%
      mutate(group = "spore")

    if (nrow(spore_dat) != 0) {
      df_list[[i]] <- bind_rows(pollen_dat, spore_dat) %>%
        cbind(station_info)
    } else {
      df_list[[i]] <- pollen_dat %>%
        cbind(station_info)
    }
  }
  print(i)
}
df_all <- bind_rows(df_list) %>% as_tibble()
write_rds(df_all, str_c(.path$nab_clean, "nab_dat_20230322.rds"))
