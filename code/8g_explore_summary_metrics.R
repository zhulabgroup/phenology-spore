# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

df_summary <- df_metrics %>% 
  mutate(ln_peak = log(peak + 1)) %>% 
  mutate(ln_integral = log(integral + 1)) %>% 
  mutate(ln_integral_as = log(integral_as + 1)) %>% 
  mutate(peak = ifelse(peak_check == 1, peak, NA)) %>% 
  gather(key = "Metric", value = "Value", peak, ln_peak, peak_doy, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("sas", "eas"), 1, observ_pct)) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("las", "integral_as", "ln_integral_as"), observ_pct_as, observ_pct)) %>% 
  filter(observ_pct >= pct) %>% 
  filter(Metric %in% c("ln_peak", "ln_integral", "ln_integral_as", "peak_doy", "sos", "eos", "los", "sas", "eas", "las")) %>% 
  mutate(Metric = ifelse(Metric == "ln_peak", "peak", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "ln_integral", "integral", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "ln_integral_as", "as integral", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "peak_doy", "peak date", Metric))
df_summary$Metric <- factor(df_summary$Metric, levels = c("peak", "peak date", "integral", "sos", "eos", "los", "sas", "eas", "las", "as integral"))

labels_e <- function(x) {parse(text = gsub("e^", x))}
p_metrics_1 <- ggplot(data = df_summary %>% filter(Metric %in% c("peak", "integral", "as integral")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Spore concentration\n(grains / m^3)") +
  scale_y_continuous(
    breaks = seq(5, 17, by = 2),
    labels = scales::math_format(e^.x)
    ) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"))

p_metrics_2 <- ggplot(data = df_summary %>% filter(Metric %in% c("peak date", "sos", "eos", "los", "sas", "eas", "las")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Day of year") +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"))

p_metrics <- plot_grid(
  p_metrics_1, p_metrics_2,
  labels = c("A", "B"),
  ncol = 2,
  align = "v",
  rel_widths = c(1, 2)
  )