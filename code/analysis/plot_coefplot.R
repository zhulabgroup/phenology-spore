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
    "intst"))

# set y axis order
df$metric <- factor(df$metric, levels = c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln(Ca)", "ln(Cp)", "ln(AIn)", "ln(ASIn)"))
df$x_variable <- factor(df$x_variable, levels = c("year", "MAT", "TAP"))
# plot

p_coef_list <- list()
for (x_var in c("year", "MAT", "TAP")) {
  for (metric_type in c("pheno", "intst")) {
    df_coef <- df %>% 
      filter(x_variable == x_var) %>% 
      filter(mtype == metric_type)
    xlims <- max(max(abs(df_coef$ci1)), max(abs(df_coef$ci2)))
    
    out_gg <- ggplot(df_coef) +
      geom_vline(aes(xintercept = 0), col = "grey") +
      geom_point(
        data = df_coef %>% filter(p < 0.05),
        aes(x = beta, y = interaction(metric, x_variable), col = x_variable)) +
      geom_point(
        data = df_coef %>% filter(p >= 0.05),
        aes(x = beta, y = interaction(metric, x_variable), col = x_variable), alpha = 0.7, shape = 1) +
      geom_errorbar(
        data = df_coef %>% filter(p < 0.05),
        aes(xmin = ci1, xmax = ci2, y = interaction(metric, x_variable), col = x_variable),
        show.legend = FALSE,
        width = 0) +
      geom_errorbar(
        data = df_coef %>% filter(p >= 0.05),
        aes(xmin = ci1, xmax = ci2, y = interaction(metric, x_variable), col = x_variable, alpha = 0.7),
        show.legend = FALSE,
        width = 0) +
      geom_text(
        data = df_coef %>% filter(p < 0.05),
        x = xlims,
        aes(
          y = interaction(metric, x_variable),
          label = paste0(signif(beta, digits = 2), " (", signif(ci1, digits = 2), ", ", signif(ci2, digits = 2), ")"),
          col = x_variable),
        vjust = -1, hjust = 1,
        show.legend = FALSE) +
      geom_text(
        data = df_coef %>% filter(p >= 0.05),
        x = xlims,
        aes(
          y = interaction(metric, x_variable),
          label = paste0(signif(beta, digits = 2), " (", signif(ci1, digits = 2), ", ", signif(ci2, digits = 2), ")"),
          col = x_variable),
        alpha = 0.7,
        vjust = -1, hjust = 1,
        show.legend = FALSE) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
      theme_bw() +
      scale_x_continuous(limits = c(-xlims, xlims)) +
      scale_color_manual(
        name = "x variable",
        values = c("year" = "dark green", "MAT" = "dark orange", "TAP" = "dark blue"),
        breaks = c("year", "MAT", "TAP")) +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "grey"),
        strip.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom",
        text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
      guides(color = "none")
      
    if (x_var == "year") {
      out_gg <- out_gg +
        theme(strip.text.y.left = element_text(angle = 0, color = "black"))
    } else {
      out_gg <- out_gg +
        theme(strip.text = element_blank())
    }
    
    p_coef_list <- c(p_coef_list, list(out_gg))
  }
}



p_coef_r1 <- p_coef_list[[1]] +
  labs(title = "Independent variable: year") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5)) +
p_coef_list[[3]] +
  labs(title = "Independent variable: MAT") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5)) + 
p_coef_list[[5]] +
  labs(title = "Independent variable: TAP") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))

p_coef_r2 <- p_coef_list[[2]] +
  p_coef_list[[4]] +
  labs(x = expression(italic(beta)[1])) + 
  p_coef_list[[6]]

p_coef <- p_coef_r1 / p_coef_r2 + plot_layout(heights = c(3, 2))