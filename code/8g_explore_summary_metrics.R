# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))


# annotation
df_antt <- df_smooth %>% 
  filter(n == 10) %>% 
  filter(year == 2009 | (year == 2010 & doy <= offset*2)) %>% 
  left_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))


p_antt <- ggplot() +
  geom_ribbon_pattern(
    data = df_antt %>% filter(year_new == 2009 & doy_new %in% 137:236),
    aes(x = date, ymin = 0, ymax = count_whit, pattern_colour = "allergy season integral"),
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
    aes(x = date, ymin = 0, ymax = count_whit, fill = "annual integral"),
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
    aes(x = as_datetime("2009-01-01"), y = 7955.968, label = "peak concentration"),
    hjust = 0,
    vjust = -0.5,
    col = "dark red"
  ) +
  geom_segment(
    aes(x = as_datetime("2009-08-12"), xend = as_datetime("2009-08-12"), y = 0, yend = 7955.968),
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
    aes(x = as_datetime("2009-02-11"), y = exp((log(1333.443) + log(7955.968)) / 2), label = "amplitude"),
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
    aes(x = as_datetime("2009-08-16"), y = exp(9.25), label = "length of allergy season"),
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
    aes(x = as_datetime("2009-08-09"), y = exp(9.5), label = "length of spore season"),
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
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_datetime(
    breaks = c(as_datetime("2009-02-10"), as_datetime("2009-04-29"), as_datetime("2009-06-27"), as_datetime("2009-08-12"), as_datetime("2009-10-04"), as_datetime("2009-11-19"), as_datetime("2010-02-10")), 
    date_labels = c("1", "start of spore season", "start of allergy season", "day of peak concentration", "end of allergy season", "end of spore season", "365")
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
  gather(key = "Metric", value = "Value", peak, ln_peak, peak_doy, amplitude, ln_amplitude, integral, ln_integral, sos, eos, los, sas, eas, las, integral_as, ln_integral_as) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("sas", "eas"), 1, observ_pct)) %>% 
  mutate(observ_pct = ifelse(Metric %in% c("las", "integral_as", "ln_integral_as"), observ_pct_as, observ_pct)) %>% 
  filter(observ_pct >= pct) %>% 
  filter(Metric %in% c("ln_peak", "ln_amplitude", "ln_integral", "ln_integral_as", "peak_doy", "sos", "eos", "los", "sas", "eas", "las")) %>% 
  mutate(Metric = ifelse(Metric == "ln_peak", "peak", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "ln_amplitude", "amplitude", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "ln_integral", "integral", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "ln_integral_as", "as integral", Metric)) %>% 
  mutate(Metric = ifelse(Metric == "peak_doy", "peak\ndoy", Metric))
df_summary$Metric <- factor(df_summary$Metric, levels = c("peak", "amplitude", "integral", "as integral", "las", "eas", "sas", "los", "eos", "sos", "peak\ndoy"))

labels_e <- function(x) {parse(text = gsub("e^", x))}
p_metrics_b <- ggplot(data = df_summary %>% filter(Metric %in% c("peak", "amplitude")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.2) +
  theme_classic() +
  ylab(expression("Spore concentration (grains*m"^-3*")")) +
  scale_y_continuous(
    breaks = seq(5, 17, by = 2),
    labels = scales::math_format(e^.x)
    ) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(title = expression(paste(bold("B")))) +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10))

p_metrics_c <- ggplot(data = df_summary %>% filter(Metric %in% c("peak\ndoy", "sos", "eos", "los", "sas", "eas", "las")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Day of year") +
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

p_metrics_d <- ggplot(data = df_summary %>% filter(Metric %in% c("integral", "as integral")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.4) +
  theme_classic() +
  ylab(expression("Spore concentration * days (grains*m"^-3*" * days)")) +
  scale_y_continuous(
    breaks = seq(5, 17, by = 2),
    labels = scales::math_format(e^.x)
  ) +
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
