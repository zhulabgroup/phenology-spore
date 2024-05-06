source("~/Github/spore_phenology/code/analysis/calc_lme.R")
# generate the dataframe
df_m <- data.frame()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  for (x in c("year_new", "MAT", "TAP")) {
    m_rslt <- calc_lme(df_in = df_ana, metric = m_metric, x_vrb = x, pct = 0.8)
    df_m <-rbind(df_m, m_rslt)
  }
}
colnames(df_m) <- c("metric", "cpltness", "n_obsv", "change", "x_variable", "beta", "ci1", "ci2", "p")
df <- df_m %>% 
  mutate(beta = as.numeric(beta)) %>% 
  mutate(ci1 = as.numeric(ci1)) %>% 
  mutate(ci2 = as.numeric(ci2)) %>% 
  mutate(p = as.numeric(p)) %>% 
  mutate(pspct = ifelse(
    metric %in% c("SOS", "EOS", "LOS", "ln_Ca", "ln_AIn"),
    "Ecology Perspective",
    "Public Health Perspective")) %>% 
  mutate(metric = case_when(
    metric == "ln_Ca" ~ "ln(Ca)",
    metric == "ln_Cp" ~ "ln(Cp)",
    metric == "ln_AIn" ~ "ln(AIn)",
    metric == "ln_ASIn" ~ "ln(ASIn)",
    T ~ metric)) %>% 
  mutate(x_variable = ifelse(x_variable == "year_new", "year", x_variable)) %>% 
  mutate(mtype = ifelse(
    metric %in% c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS"),
    "pheno",
    "intst")) %>% 
  mutate(transparency = ifelse(p < 0.05, 1, 0.5))

p_coef_list <- list()
for (x_var in c("year", "MAT", "TAP")) {
  for (metric_type in c("pheno", "intst")) {
    df_coef <- df %>% 
      filter(x_variable == x_var) %>% 
      filter(mtype == metric_type)
    xlims <- max(max(abs(df_coef$ci1)), max(abs(df_coef$ci2)))
    
    if (metric_type == "pheno") {
      df_coef$metric <- factor(df_coef$metric, levels = c("LAS", "LOS", "EAS", "EOS", "SAS", "SOS"))
    } else {
      df_coef$metric <- factor(df_coef$metric, levels = c("ln(ASIn)", "ln(AIn)", "ln(Cp)", "ln(Ca)"))
    }
    
    out_gg <- ggplot(df_coef) +
      geom_vline(aes(xintercept = 0), col = "grey") +
      geom_errorbar(
        aes(xmin = ci1, xmax = ci2, y = metric, col = x_variable, alpha = transparency),
        width = 0) +
      geom_point(
        aes(x = beta, y = metric, col = x_variable, alpha = transparency)) +
      geom_text(
        # data = df_coef %>% filter(p < 0.05),
        x = xlims,
        aes(
          y = metric,
          label = paste0(signif(beta, digits = 2), " (", signif(ci1, digits = 2), ", ", signif(ci2, digits = 2), ")"),
          col = x_variable, alpha = transparency),
        vjust = -1, hjust = 1) +
      # facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
      scale_x_continuous(limits = c(-xlims, xlims)) +
      scale_color_manual(
        name = "x variable",
        values = c("year" = "dark green", "MAT" = "dark orange", "TAP" = "dark blue"),
        breaks = c("year", "MAT", "TAP")) +
      scale_alpha_identity() +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
      guides(color = "none")
      
    if (x_var != "year") {
      out_gg <- out_gg +
        theme(axis.text.y = element_blank())
    }
    
    p_coef_list <- c(p_coef_list, list(out_gg))
  }
}

p_coef <- 
  (p_coef_list[[1]] +
             labs(title = "Independent variable: year") +
             theme(plot.title = element_text(face = "plain", hjust = 0.5)) +
             p_coef_list[[3]] +
             labs(title = "Independent variable: MAT") +
             theme(plot.title = element_text(face = "plain", hjust = 0.5)) + 
             p_coef_list[[5]] +
             labs(title = "Independent variable: TAP") +
             theme(plot.title = element_text(face = "plain", hjust = 0.5))) / 
  (p_coef_list[[2]] +
     p_coef_list[[4]] +
     labs(x = expression(italic(beta)[1])) + 
     p_coef_list[[6]]) + 
  plot_layout(heights = c(3, 2))