source("~/Github/spore_phenology/code/analysis/tidy_smoothfillwhit_station.R")
source("~/Github/spore_phenology/code/analysis/tidy_sporeyr_station.R")
source("~/Github/spore_phenology/code/analysis/calc_completeness_stationspyr.R")
source("~/Github/spore_phenology/code/analysis/calc_metrics_stationspyr.R")
source("~/Github/spore_phenology/code/analysis/tidy_gathermetrics.R")
source("~/Github/spore_phenology/code/analysis/calc_lme.R")

ana_sens_l <- function(df_in, lmd) {
  df_smooth_lmd <- tidy_smoothfillwhit_station(df_raw = df_in, n_gap = 14, lambda = lmd,column_name = count, new_column_name = count_fillwhit)
  
  df_sporeyr = df_smooth_lmd %>% 
    tidy_sporeyr_station()
  
  df_spore_cpltness <- calc_completeness_stationspyr(df_sporeyr, column_name = count_fillwhit)
  
  df_metrics <- calc_metrics_stationspyr(df_completeness = df_spore_cpltness, df_raw = df_sporeyr)
  
  df_metrics_long <- tidy_gathermetrics(df_metrics)
  
  df_m <- data.frame()
  for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
      m_rslt <- calc_lme(df_in = df_metrics_long, metric = m_metric, x_vrb = "year_new", pct = 0.8)
      df_m <-rbind(df_m, m_rslt)
  }
  colnames(df_m) <- c("metric", "cpltness", "n_obsv", "change", "x_variable", "beta", "ci1", "ci2", "p")
  df_m <- df_m %>% 
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
    mutate(lambda = lmd)
  
  return(df_m)
}

df_sens_l <- data.frame()
for (l in c(10, seq(50, 500, by = 50))) {
  df <- ana_sens_l(df_in = df_full, lmd = l)
  df_sens_l <- df_sens_l %>% rbind(df)
}

df_sens_l_slope <- df_sens_l %>%
  group_by(metric) %>%
  do({
    result <- lm(beta ~ lambda, .)
    data_frame(
      r_squared =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value =
        result %>%
        anova() %>%
        magrittr::use_series(`Pr(>F)`) %>%
        magrittr::extract2(1)
    ) %>%
      bind_cols(
        result %>%
          coef() %>%
          as.list() %>%
          as_data_frame()
      )
  }) %>%
  rename("slope" = "lambda", "intercept" = "(Intercept)") %>%
  ungroup() %>%
  right_join(df_sens_l, by = "metric") %>% 
  mutate(facet_label = paste0("slope = ", round(slope, 5), "  *p* = ", round(p_value, 5)))

df_sens_l_slope$metric <- factor(df_sens_l_slope$metric, levels = c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln(Ca)", "ln(Cp)", "ln(AIn)", "ln(ASIn)"))

p_sens_l <- ggplot(data = df_sens_l_slope, aes(x = lambda, y = beta)) +
  geom_hline(aes(yintercept = 0), col = "gray") +
  geom_point(col = "dark red") +
  geom_errorbar(aes(ymin = ci1, ymax = ci2), col = "dark red", width =0) +
  scale_x_continuous(breaks = c(10, seq(50, 500, by = 50))) +
  xlab(expression(italic(lambda))) +
  ylab(expression(italic(beta))) +
  facet_wrap(~ metric + facet_label, ncol = 2, scales = "free") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = ggtext::element_markdown(hjust = 0),
    text = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5))