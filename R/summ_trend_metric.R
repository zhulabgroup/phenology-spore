source(str_c(.path$ana_fun, "calc_lme.R"))

df_median <- df_ana %>%
  group_by(Metric) %>%
  summarise(
    med = median(Value),
    n_station = length(unique(id))
  ) %>%
  filter(Metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn"))

df_m <- data.frame()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  m_rslt <- calc_lme(df_in = df_ana, metric = m_metric, x_vrb = "year_new", pct = 0.8)
  df_m <- rbind(df_m, m_rslt)
}
colnames(df_m) <- c("Metric", "cpltness", "n_obsv", "change", "x_variable", "beta", "ci1", "ci2", "p")

df_summ_metric <- df_m %>%
  full_join(df_median, by = "Metric") %>%
  mutate(cpltness = as.numeric(cpltness)) %>%
  mutate(n_obsv = as.numeric(n_obsv)) %>%
  mutate(change = as.numeric(change)) %>%
  mutate(beta = as.numeric(beta)) %>%
  mutate(ci1 = as.numeric(ci1)) %>%
  mutate(ci2 = as.numeric(ci2)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(Metric = case_when(
    Metric == "ln_Ca" ~ "ln(Ca)",
    Metric == "ln_Cp" ~ "ln(Cp)",
    Metric == "ln_AIn" ~ "ln(AIn)",
    Metric == "ln_ASIn" ~ "ln(ASIn)",
    T ~ Metric
  )) %>%
  mutate(x_variable = ifelse(x_variable == "year_new", "year", x_variable)) %>%
  mutate(cpltness = paste0(cpltness * 100, "%")) %>%
  mutate(n_obsv = n_obsv %>% as.character()) %>%
  mutate(change_latent = ifelse(
    Metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS"),
    paste0(change %>% round(2), "d"),
    paste0((change * 100) %>% round(2), "%")
  )) %>%
  mutate(change = ifelse(
    change > 0,
    paste0("+", change_latent),
    change_latent
  )) %>%
  mutate(beta = beta %>% round(4) %>% as.character()) %>%
  mutate(p = p %>% round(4) %>% as.character()) %>%
  rename(
    "Number of stations" = "n_station",
    "Data completeness" = "cpltness",
    "Number of observations" = "n_obsv",
    "Slope" = "beta",
    "Change" = "change",
    "p-value" = "p"
  ) %>%
  dplyr::select("Metric", "Number of stations", "Number of observations", "Slope", "Change", "p-value")
