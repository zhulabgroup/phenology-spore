df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_daymet.rds"))

data <- df_daymet %>% 
  drop_na(count_whit) %>% 
  filter(state != "PR") %>% 
  filter(country == "US")

lme <- lme(
  count_whit ~ temp * prcp,
  data = data,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme)



## peak
data_peak <- df_metrics %>% 
  filter(peak_check == 1) %>%
  drop_na(peak) %>% 
  filter(observ_pct >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_peak %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_climate_peak <- lme(
  # log(peak) ~ mat + tap,
  peak_doy ~ mat + tap,
  data = data_peak,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_climate_peak)





## integral
data_integral <- df_metrics %>% 
  drop_na(integral) %>% 
  filter(observ_pct >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_integral %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_climate_integral <- lme(
  # integral ~ mat * tap,
  log(integral) ~ mat + tap,
  data = data_integral,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_climate_integral)





## sos
data_sos <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sos %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_climate_sos <- lme(
  los ~ mat + tap,
  data = data_sos,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_climate_sos)





## sas
data_sas <- df_metrics %>% 
  drop_na(eas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sas %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_sas <- lme(
  eas ~ mat + tap,
  data = data_sas,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sas)
## las
data_las <- df_metrics %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_las %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_las <- lme(
  las ~ mat + tap,
  data = data_las,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_las)





## as integral
data_integral_as <- df_metrics %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= 0.8) %>%
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>%
  ungroup()
# view(data_integral_as %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_integral_as <- lme(
  log(integral_as) ~ mat + tap,
  data = data_integral_as,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_integral_as)
