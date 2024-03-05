source("~/Github/spore_phenology/code/analysis/calc_lme.R")
# generate the dataframe
df_m <- data.frame()
for (m_metric in c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")) {
  for (x in c("year_new", "MAT", "TAP")) {
    m_rslt <- calc_lme(df_in = df_ana, metric = m_metric, x_vrb = x, pct = 0.8)
    df_m <-rbind(df_m, m_rslt)
  }
}
colnames(df_m) <- c("metric", "x_variable", "beta", "ci1", "ci2", "p")
df_m <- df_m %>% 
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
  mutate(x_variable = ifelse(x_variable == "year_new", "year", x_variable))
# set y axis order
df_m$metric <- factor(df_m$metric, levels = c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln(Ca)", "ln(Cp)", "ln(AIn)", "ln(ASIn)"))
df_m$x_variable <- factor(df_m$x_variable, levels = c("TAP", "MAT", "year"))
# plot
p_coef1 <- ggplot(subset(df_m, pspct == "Ecology Perspective")) +
  geom_vline(aes(xintercept = 0), col = "grey") +
  geom_point(aes(x = beta, y = interaction(metric, x_variable), col = x_variable), show.legend = TRUE) +
  geom_errorbar(aes(xmin = ci1, xmax = ci2, y = interaction(metric, x_variable), col = x_variable), show.legend = FALSE) +
  geom_text(
    aes(
      x = max(df_m$ci2) + 4, y = interaction(metric, x_variable),
      label = ifelse(p < 0.05, sprintf("* %0.2f (%0.2f, %0.2f)", beta, ci1, ci2), sprintf("%0.2f (%0.2f, %0.2f)", beta, ci1, ci2)),
      col = x_variable),
    vjust = 0.5, hjust = 1,
    show.legend = FALSE) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  theme_bw() +
  labs(
    x = expression(italic(beta)[1]),
    y = NULL,
    title = "Ecology Perspective") +
  scale_x_continuous(limits = c(min(df_m$ci1), max(df_m$ci2) + 4)) +
  scale_color_manual(
    name = "x variable",
    values = c("year" = "dark green", "MAT" = "dark orange", "TAP" = "dark blue"),
    breaks = c("year", "MAT", "TAP")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.border = element_rect(color = "grey"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text.y.left = element_text(angle = 0, color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    text = element_text(size = 12),
    legend.text = element_text(size = 12))

p_coef2 <- ggplot(subset(df_m, pspct == "Public Health Perspective")) +
  geom_vline(aes(xintercept = 0), col = "grey") +
  geom_point(aes(x = beta, y = interaction(metric, x_variable), col = x_variable), show.legend = TRUE) +
  geom_errorbar(aes(xmin = ci1, xmax = ci2, y = interaction(metric, x_variable), col = x_variable), show.legend = FALSE) +
  geom_text(
    aes(
      x = max(df_m$ci2) + 4, y = interaction(metric, x_variable),
      label = ifelse(p < 0.05, sprintf("* %0.2f (%0.2f, %0.2f)", beta, ci1, ci2), sprintf("%0.2f (%0.2f, %0.2f)", beta, ci1, ci2)),
      col = x_variable),
    vjust = 0.5, hjust = 1,
    show.legend = FALSE) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  theme_bw() +
  labs(
    x = expression(italic(beta)[1]),
    y = NULL,
    title = "Public Health Perspective") +
  scale_x_continuous(limits = c(min(df_m$ci1), max(df_m$ci2) + 4)) +
  scale_color_manual(
    name = "x variable",
    values = c("year" = "dark green", "MAT" = "dark orange", "TAP" = "dark blue"),
    breaks = c("year", "MAT", "TAP")) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.border = element_rect(color = "grey"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text.y.left = element_text(angle = 0, color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    text = element_text(size = 12),
    legend.text = element_text(size = 12))

legend_grob <- get_legend(p_coef1)
p_coef_r1 <- plot_grid(p_coef1 + theme(legend.position = 'none'), p_coef2 + theme(legend.position = 'none'), ncol = 2, align = "v", rel_widths = c(1, 1))
p_coef <- plot_grid(p_coef_r1, legend_grob, ncol = 1, rel_heights = c(10, 1))