# 55 stations included in our analysis
df_station <- df_spore %>% 
  left_join(
    df_full %>% 
      distinct(n, .keep_all = T) %>% 
      dplyr::select(station, state, n),
    by = c("station", "state")) %>% 
  drop_na(n)

# calculate relative abundance for each family per year
df_comm <- df_station %>% 
  filter(family != "Total" | is.na(family)) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>% 
  group_by(lat, lon, station, city, state, country, id, n, year) %>% 
  summarise(total = sum(count, na.rm = T)) %>% 
  ungroup() %>% 
  right_join(
    df_station %>% mutate(year = format(date, "%Y") %>% as.integer()),
    by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "year")) %>% 
  mutate(family = ifelse(is.na(family), "Unidentified", family)) %>% 
  group_by(lat, lon, station, city, state, country, id, n, year, total, family) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  mutate(pctg = count / total) %>% 
  arrange(desc(pctg)) %>% 
  ungroup()

# fit lme
calc_trend_taxa <- function(df_in, fml) {
  m_taxa <- lme(
    pctg ~ year,
    data = df_in %>% filter(family == fml) %>% filter(total != 0),
    random = ~ year | n, control = lmeControl(opt = "optim"))
  
  sum_m <- summary(m_taxa)
  beta0SD <- diag(sqrt(getVarCov(m_taxa)))[["(Intercept)"]] %>% as.numeric() %>% signif(digits = 2)
  beta1SD <- diag(sqrt(getVarCov(m_taxa)))[["year"]] %>% as.numeric() %>% signif(digits = 2)
  rsdlSD <- sum_m$sigma %>% as.numeric() %>% signif(digits = 2)
  b0 <- fixef(m_taxa)[["(Intercept)"]] %>% as.numeric() %>% signif(digits = 2)
  b1 <- fixef(m_taxa)[["year"]] %>% as.numeric() %>% signif(digits = 2)
  CI <- intervals(m_taxa, which = "fixed")
  CI1 <- CI$fixed[2, "lower"] %>% as.numeric() %>% signif(digits = 2)
  CI2 <- CI$fixed[2, "upper"] %>% as.numeric() %>% signif(digits = 2)
  b0SE <- sum_m$tTable["(Intercept)", "Std.Error"] %>% as.numeric() %>% signif(digits = 2)
  b1SE <- sum_m$tTable["year", "Std.Error"] %>% as.numeric() %>% signif(digits = 2)
  b0t <- sum_m$tTable["(Intercept)", "t-value"] %>% as.numeric() %>% signif(digits = 2)
  b1t <- sum_m$tTable["year", "t-value"] %>% as.numeric() %>% signif(digits = 2)
  p <- sum_m$tTable[["year", "p-value"]] %>% as.numeric() %>% signif(digits = 2)
  n_obsv <- nobs(m_taxa)
  # # compute the change
  # y2003 <- b0 + b1 * 2003
  # y2022 <- b0 + b1 * 2022
  # if (metric %in% c("ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  #   change <- ((exp(y2022) - 1) - (exp(y2003) - 1)) / (exp(y2003) - 1)
  # } else {
  #   change <- y2022 - y2003
  # }
  # Create a list
  result <- c(fml, beta0SD, beta1SD, rsdlSD, b0, b1, b0SE, b1SE, b0t, b1t, p)
  return(result)
}

# generate table
df_trend_taxa <- data.frame()
for (fam in c("Unidentified", "Cladosporiaceae")) {
  m_rslt <- calc_trend_taxa(df_in = df_comm, fml = fam)
  df_trend_taxa <-rbind(df_trend_taxa, m_rslt)
}
colnames(df_trend_taxa) <- c("Taxa", "beta0SD", "beta1SD", "rsdlSD", "b0", "b1", "b0SE", "b1SE", "b0t", "b1t", "p")