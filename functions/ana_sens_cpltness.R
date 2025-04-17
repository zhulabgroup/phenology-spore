source("~/Github/spore_phenology/code/analysis/calc_lme.R")

df_m <- data.frame()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  for (cplt in c(1, 0.9, 0.8, 0.7, 0.6)) {
    m_rslt <- calc_lme(df_in = df_ana_full, metric = m_metric, x_vrb = "year_new", pct = cplt)
    df_m <-rbind(df_m, m_rslt)
  }
}
colnames(df_m) <- c("metric", "cpltness", "n_obsv", "change", "x_variable", "beta", "ci1", "ci2", "p")

df_sens_cpltness <- df_m %>% 
  mutate(cpltness = as.numeric(cpltness)) %>% 
  mutate(n_obsv = as.numeric(n_obsv)) %>% 
  mutate(change = as.numeric(change)) %>% 
  mutate(beta = as.numeric(beta)) %>% 
  mutate(ci1 = as.numeric(ci1)) %>% 
  mutate(ci2 = as.numeric(ci2)) %>% 
  mutate(p = as.numeric(p)) %>% 
  mutate(metric = case_when(
    metric == "ln_Ca" ~ "ln(Ca)",
    metric == "ln_Cp" ~ "ln(Cp)",
    metric == "ln_AIn" ~ "ln(AIn)",
    metric == "ln_ASIn" ~ "ln(ASIn)",
    T ~ metric)) %>% 
  mutate(x_variable = ifelse(x_variable == "year_new", "year", x_variable)) %>% 
  mutate(cpltness = paste0(cpltness * 100, "%")) %>% 
  mutate(n_obsv = n_obsv %>% as.character()) %>% 
  mutate(change_latent = ifelse(
    metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS"),
    paste0(change %>% round(2), "d"),
    paste0((change * 100) %>% round(2), "%"))) %>% 
  mutate(change = ifelse(
    change > 0,
    paste0("+", change_latent),
    change_latent)) %>% 
  mutate(beta = beta %>% round(4) %>% as.character()) %>% 
  mutate(p = p %>% round(4) %>% as.character()) %>% 
  rename(
    "Metric" = "metric",
    "Data completeness" = "cpltness",
    "Number of observations" = "n_obsv",
    "Slope" = "beta",
    "Change" = "change",
    "p-value" = "p"
    ) %>% 
  dplyr::select("Metric", "Data completeness", "Number of observations", "Slope", "Change", "p-value")
