# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))


# annotation
df_antt <- df_smooth %>% 
  filter(n == 10) %>% 
  filter(year == 2009 | (year == 2010 & doy <= offset*2)) %>% 
  left_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))


p_antt <- ggplot() +
  geom_ribbon_pattern(
    data = df_antt %>% filter(year_new == 2009 & doy_new %in% 137:236),
    aes(x = date, ymin = 0, ymax = count_whit, pattern_colour = "allergy season integral\n(ASIn)"),
    pattern = "stripe",
    pattern_fill = "dark red",
    fill = NA,
    alpha = 0.4
  ) +
  scale_pattern_colour_manual(
    name = "",
    values = c("dark red"),
    guide = guide_legend(alpha = 0.4)
  ) +
  geom_ribbon_pattern(
    data = df_antt %>% filter(year_new == 2009 & doy_new %in% 1:78),
    aes(x = date, ymin = 0, ymax = count_whit, pattern_fill = "10% of the annual integral"),
    pattern = "stripe",
    pattern_colour = "orange",
    fill = NA
  ) +
  scale_pattern_fill_manual(
    name = "",
    values = c("orange"),
    guide = guide_legend(alpha = 0.4)
  ) +
  geom_ribbon_pattern(
    data = df_antt %>% filter(year_new == 2009 & doy_new %in% 282:365),
    aes(x = date, ymin = 0, ymax = count_whit),
    pattern = "stripe",
    pattern_colour = "orange",
    pattern_fill = "orange",
    fill = NA,
    show.legend = F
  ) +
  geom_ribbon(
    data = df_antt %>% filter(year_new == 2009 & doy_new %in% 1:365),
    aes(x = date, ymin = 0, ymax = count_whit, fill = "annual integral\n(AIn)"),
    alpha = 0.4
  ) +
  scale_fill_manual(
    name='',
    values= "orange",
    guide = guide_legend(override.aes = list(alpha = 0.4))
  ) +
  geom_segment(
    data = df_antt %>% filter(year_new == 2009) %>% filter(doy_new %in% c(1, 365)),
    aes(x = date, xend = date, y = 0, yend = count_whit),
    col = "black"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-08-12"), y = 7955.968, yend = 7955.968),
    col = "dark red"
  ) +
  geom_text(
    aes(x = as_datetime("2009-01-01"), y = 7955.968, label = "peak concentration (Cp)"),
    hjust = 0,
    vjust = -0.5,
    col = "dark red"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-02-11"), y = 1333.443, yend = 1333.443),
    col = "orange",
    linetype = "dashed"
  ) +
  geom_text(
    aes(x = as_datetime("2009-01-01"), y = 1333.443, label = "trough"),
    hjust = 0,
    vjust = -0.5,
    col = "orange"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-02-11"), xend = as_datetime("2009-02-11"), y = 1333.443, yend = 7955.968),
    col = "orange"
  ) +
  geom_text(
    aes(x = as_datetime("2009-02-11"), y = exp((log(1333.443) + log(7955.968)) / 2), label = "amplitude (A)"),
    hjust = 0,
    vjust = -0.5,
    col = "orange"
  ) +
  geom_segment(
    data = df_antt %>% filter(doy_new == sos | doy_new == eos),
    aes(x = date, xend = date, y = 0, yend = count_whit),
    col = "orange"
  ) +
  geom_segment(
    data = df_antt %>% filter(doy_new == sas | doy_new == eas),
    aes(x = date, xend = date, y = 0, yend = count_whit),
    col = "dark red"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-10-04"), y = 6500, yend = 6500),
    linetype = "dashed",
    col = "dark red"
  ) +
  geom_text(
    aes(x = as_datetime("2009-01-01"), y = 6500, label = "NAB allergy shreshold"),
    hjust = 0,
    vjust = 1.3,
    col = "dark red"
  ) +
  geom_segment(
    data = df_antt %>% filter(doy_new == sas | doy_new == eas),
    aes(x = date, xend = date, y = count_whit, yend = exp(9.25)),
    linetype = "dashed",
    col = "dark red"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-06-27"), xend = as_datetime("2009-10-04"), y = exp(9.25), yend = exp(9.25)),
    col = "dark red"
  ) +
  geom_text(
    aes(x = as_datetime("2009-08-16"), y = exp(9.25), label = "length of allergy season (LAS)"),
    hjust = 0.5,
    vjust = -0.3,
    col = "dark red"
  ) +
  geom_segment(
    data = df_antt %>% filter(doy_new == sos | doy_new == eos),
    aes(x = date, xend = date, y = count_whit, yend = exp(9.5)),
    linetype = "dashed",
    col = "orange"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-04-29"), xend = as_datetime("2009-11-19"), y = exp(9.5), yend = exp(9.5)),
    col = "orange"
  ) +
  geom_text(
    aes(x = as_datetime("2009-08-09"), y = exp(9.5), label = "length of spore season (LOS)"),
    hjust = 0.5,
    vjust = -0.3,
    col = "orange"
  ) +
  geom_path(
    data = df_antt %>% filter(year_new == 2009),
    aes(x = date, y = count_whit),
    col = "black"
  ) +
  geom_path(
    data = df_antt %>% filter(year_new == 2008),
    aes(x = date, y = count_whit),
    linetype = "dashed",
    col = "black"
  ) +
  geom_path(
    data = df_antt %>% filter(year_new == 2010),
    aes(x = date, y = count_whit),
    linetype = "dashed",
    col = "black"
  ) +
  geom_point(
    aes(x = as_datetime("2009-08-12"), y = 7955.968),
    col = "dark red"
  ) +
  geom_text(
    aes(x = as_datetime("2009-01-01"), y = exp(7), label = "offset"),
    hjust = 0,
    vjust = 1,
    col = "black"
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # annotation_logticks(sides = "l") +
  scale_x_datetime(
    breaks = c(as_datetime("2009-02-10"), as_datetime("2009-04-29"), as_datetime("2009-06-27"), as_datetime("2009-10-04"), as_datetime("2009-11-19"), as_datetime("2010-02-10")), 
    date_labels = c("1", "start of spore season (SOS)", "start of allergy season (SAS)", "end of allergy season (EAS)", "end of spore season (EOS)", "365")
  ) +
  coord_cartesian(
    xlim = c(as_datetime("2009-01-21"), max(df_antt$date)),
    ylim = c(exp(7), exp(10))
  ) +
  ylab(expression("Spore concentration (grains m"^-3*")")) +
  xlab("Day of spore year") +
  theme_classic() +
  theme(
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "left"
  ) +
  theme(
    axis.text.x = element_text(
      color = "black",
      angle = 15,
      hjust = 1
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("A")))) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

# summary of metrics
df_summary <- df_metrics %>% 
  mutate(ln_peak = log(peak + 1)) %>% 
  mutate(ln_integral = log(integral + 1)) %>% 
  mutate(ln_integral_as = log(integral_as + 1)) %>% 
  mutate(ln_amplitude = log(amplitude + 1)) %>% 
  mutate(peak = ifelse(peak_check == 1, peak, NA)) %>% 
  mutate(sos = ifelse(observ_pct >= 1, sos, NA)) %>% 
  mutate(eos = ifelse(observ_pct >= 1, eos, NA)) %>% 
  mutate(los = ifelse(observ_pct >= 1, los, NA)) %>% 
  gather(key = "Metric", value = "Value", peak, ln_peak, peak_doy, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("sas", "eas", "las"), 1, observ_pct)) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("integral_as", "ln_integral_as"), observ_pct_as, observ_pct)) %>% 
  filter(observ_pct >= pct) %>% 
  filter(Metric %in% c("peak", "amplitude", "integral", "integral_as", "sos", "eos", "los", "sas", "eas", "las")) %>% 
  mutate(Metric = ifelse(Metric == "sos", "SOS", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "eos", "EOS", Metric)) %>%
  mutate(Metric = ifelse(Metric == "los", "LOS", Metric)) %>%
  mutate(Metric = ifelse(Metric == "sas", "SAS", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "eas", "EAS", Metric)) %>%
  mutate(Metric = ifelse(Metric == "las", "LAS", Metric)) %>%
  mutate(Metric = ifelse(Metric == "peak", "Cp", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "amplitude", "A", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "integral", "AIn", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "integral_as", "ASIn", Metric))
  
df_summary$Metric <- factor(df_summary$Metric, levels = c("Cp", "A", "AIn", "ASIn", "LAS", "EAS", "SAS", "LOS", "EOS", "SOS"))

# labels_e <- function(x) {parse(text = gsub("e^", x))}
p_metrics_b <- ggplot(data = df_summary %>% filter(Metric %in% c("Cp", "A")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration (grains m"^-3*")")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("B")))) +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10))

p_metrics_c <- ggplot(data = df_summary %>% filter(Metric %in% c("SOS", "EOS", "LOS", "SAS", "EAS", "LAS")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Day of spore year") +
  scale_y_continuous(
    limits = c(-15, 406),
    breaks = c(1, 100, 200, 300, 365),
    position = "right"
  ) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("C")))) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b = -10)),
    plot.margin = margin(0, 10, 10, 10)
    )+
  coord_flip()

p_metrics_d <- ggplot(data = df_summary %>% filter(Metric %in% c("AIn", "ASIn")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration * days (grains m"^-3*" days)")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("D")))) +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10))

p_metrics_1 <- plot_grid(p_antt,
                         p_metrics_c,
                         ncol = 1,
                         rel_heights = c(1, 1))
p_metrics_2 <- plot_grid(p_metrics_b,
                         p_metrics_d,
                         ncol = 1,
                         rel_heights = c(1, 1))
p_metrics <- plot_grid(p_metrics_1, p_metrics_2,
                       ncol = 2,
                       rel_widths = c(2, 1))
