# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

# annotation
plot_antt <- function(df_in, sample_city, spyr) {
  df_antt <- df_in %>% 
    filter(city == sample_city) %>% 
    filter(date_new %in% c((as.Date(paste0(spyr, "-01-01")) - 30) : (as.Date(paste0(spyr, "-12-31")) + 30))) %>% 
    left_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
  
  df_spyr <- df_antt %>% filter(year_new == spyr)
  sample_startdate <- min(df_antt$date)
  sample_startdate_spyr <- min(df_spyr$date)
  sample_sos <- head(df_spyr$sos, 1)
  sample_eos <- head(df_spyr$eos, 1)
  sample_sos_date_old <- head(df_spyr$sos_date_old, 1)
  sample_eos_date_old <- head(df_spyr$eos_date_old, 1)
  sample_sas <- head(df_spyr$sas, 1)
  sample_eas <- head(df_spyr$eas, 1)
  sample_sas_date_old <- head(df_spyr$sas_date_old, 1)
  sample_eas_date_old <- head(df_spyr$eas_date_old, 1)
  sample_peak_date_old <- head(df_spyr$peak_date_old, 1)
  sample_peak <- head(df_spyr$peak, 1)
  sample_trough_date_old <- head(df_spyr$trough_date_old, 1)
  sample_trough <- head(df_spyr$trough, 1)
  
  p_antt <- ggplot() +
    geom_ribbon_pattern(
      data = df_spyr %>% filter(doy_new %in% sample_sas:sample_eas),
      aes(x = date, ymin = 0, ymax = count_fillwhit, pattern_colour = "allergy season integral\n(ASIn)"),
      pattern = "stripe",
      pattern_fill = "dark red",
      fill = NA,
      alpha = 0.4) +
    scale_pattern_colour_manual(
      name = "",
      values = c("dark red"),
      guide = guide_legend(alpha = 0.4)) +
    geom_ribbon_pattern(
      data = df_spyr %>% filter(doy_new %in% 1:sample_sos),
      aes(x = date, ymin = 0, ymax = count_fillwhit, pattern_fill = "10% of the annual integral"),
      pattern = "stripe",
      pattern_colour = "orange",
      fill = NA) +
    scale_pattern_fill_manual(
      name = "",
      values = c("orange"),
      guide = guide_legend(alpha = 0.4)) +
    geom_ribbon_pattern(
      data = df_spyr %>% filter(doy_new %in% sample_eos:365),
      aes(x = date, ymin = 0, ymax = count_fillwhit),
      pattern = "stripe",
      pattern_colour = "orange",
      pattern_fill = "orange",
      fill = NA,
      show.legend = F) +
    geom_ribbon(
      data = df_spyr %>% filter(doy_new %in% 1:365),
      aes(x = date, ymin = 0, ymax = count_fillwhit, fill = "annual integral\n(AIn)"),
      alpha = 0.4) +
    scale_fill_manual(
      name='',
      values= "orange",
      guide = guide_legend(override.aes = list(alpha = 0.4))) +
    geom_segment(
      aes(x = sample_startdate, xend = sample_peak_date_old, y = sample_peak, yend = sample_peak),
      col = "dark red") +
    geom_text(
      aes(x = sample_startdate, y = sample_peak, label = "peak concentration (Cp)"),
      hjust = 0,
      vjust = -0.5,
      col = "dark red") +
    geom_segment(
      aes(x = sample_startdate, xend = sample_trough_date_old, y = sample_trough, yend = sample_trough),
      col = "orange",
      linetype = "dashed") +
    geom_text(
      aes(x = sample_startdate, y = sample_trough, label = "trough"),
      hjust = 0,
      vjust = -0.5,
      col = "orange") +
    geom_segment(
      aes(x = sample_startdate_spyr, xend = sample_startdate_spyr, y = sample_trough, yend = sample_peak),
      col = "orange") +
    geom_text(
      aes(x = sample_startdate_spyr, y = 10^((log10(sample_trough) + log10(sample_peak)) / 2), label = "amplitude (A)"),
      hjust = 0,
      vjust = -0.5,
      col = "orange") +
    geom_segment(
      data = df_spyr %>% filter(doy_new == sos | doy_new == eos),
      aes(x = date, xend = date, y = 0, yend = count_fillwhit),
      col = "orange") +
    geom_segment(
      data = df_spyr %>% filter(doy_new == sas | doy_new == eas),
      aes(x = date, xend = date, y = 0, yend = count_fillwhit),
      col = "dark red") +
    geom_segment(
      aes(x = sample_startdate, xend = sample_eas_date_old, y = 6500, yend = 6500),
      linetype = "dashed",
      col = "dark red") +
    geom_text(
      aes(x = sample_startdate, y = 6500, label = "NAB allergy shreshold"),
      hjust = 0,
      vjust = 0,
      col = "dark red") +
    geom_segment(
      data = df_spyr %>% filter(doy_new == sas | doy_new == eas),
      aes(x = date, xend = date, y = count_fillwhit, yend = 10^4.2),
      linetype = "dashed",
      col = "dark red") +
    geom_segment(
      aes(x = sample_sas_date_old, xend = sample_eas_date_old, y = 10^4.2, yend = 10^4.2),
      col = "dark red") +
    geom_text(
      aes(x = sample_sas_date_old + (sample_eas - sample_sas) / 2, y = 10^4.2, label = "length of allergy season (LAS)"),
      hjust = 0.5,
      vjust = -0.3,
      col = "dark red") +
    geom_segment(
      data = df_spyr %>% filter(doy_new == sos | doy_new == eos),
      aes(x = date, xend = date, y = count_fillwhit, yend = 10^4.3),
      linetype = "dashed",
      col = "orange") +
    geom_segment(
      aes(x = sample_sos_date_old, xend = sample_eos_date_old, y = 10^4.3, yend = 10^4.3),
      col = "orange") +
    geom_text(
      aes(x = sample_sos_date_old + (sample_eos - sample_sos) / 2, y = 10^4.3, label = "length of spore season (LOS)"),
      hjust = 0.5,
      vjust = -0.3,
      col = "orange") +
    geom_path(
      data = df_spyr,
      aes(x = date, y = count_fillwhit),
      col = "black") +
    geom_path(
      data = df_antt %>% filter(year_new == spyr - 1),
      aes(x = date, y = count_fillwhit),
      linetype = "dashed",
      col = "black") +
    geom_path(
      data = df_antt %>% filter(year_new == spyr + 1),
      aes(x = date, y = count_fillwhit),
      linetype = "dashed",
      col = "black") +
    geom_point(
      aes(x = sample_peak_date_old, y = sample_peak),
      col = "dark red") +
    geom_text(
      aes(x = sample_startdate, y = 10^3.3, label = "offset"),
      hjust = 0,
      vjust = 1,
      col = "black") +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    annotation_logticks(sides = "l") +
    scale_x_date(
      breaks = c(sample_startdate_spyr,
                 sample_sos_date_old,
                 sample_sas_date_old,
                 sample_eas_date_old,
                 sample_eos_date_old,
                 sample_startdate_spyr + 365),
      date_labels = c("1",
                      "start of spore season (SOS)",
                      "start of allergy season (SAS)",
                      "end of allergy season (EAS)",
                      "end of spore season (EOS)",
                      "365")) +
    coord_cartesian(
      xlim = c(sample_startdate_spyr, max(df_antt$date)),
      ylim = c(10^3.3, 10^4.3)
    ) +
    ylab(expression("Spore concentration (grains m"^-3*")")) +
    xlab("Day of spore year") +
    theme_classic() +
    theme(
      legend.position = c(1, 1),
      legend.justification = c("right", "top"),
      legend.box.just = "left") +
    theme(
      axis.text.x = element_text(
        color = "black",
        angle = 15,
        hjust = 1),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.y = element_text(color = "black")) +
    labs(title = expression(paste(bold("A")))) +
    theme(
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10))
  
  return(p_antt)
}

p_antt <- plot_antt(df_in = df_sporeyr, sample_city = "Sarasota", spyr = 2008)



# summary of metrics
df_ana$Metric <- factor(df_ana$Metric, levels = c("Cp", "A", "AIn", "ASIn", "LAS", "EAS", "SAS", "LOS", "EOS", "SOS"))

# labels_e <- function(x) {parse(text = gsub("e^", x))}
p_metrics_con <- ggplot(data = df_ana %>% filter(Metric %in% c("Cp", "A")), aes(x = Metric, y = Value)) +
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

p_metrics_pheno <- ggplot(data = df_ana %>% filter(Metric %in% c("SOS", "EOS", "LOS", "SAS", "EAS", "LAS")), aes(x = Metric, y = Value)) +
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

p_metrics_in <- ggplot(data = df_ana %>% filter(Metric %in% c("AIn", "ASIn")), aes(x = Metric, y = Value)) +
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

p_metrics <- plot_grid(p_antt, p_metrics_con,
                       p_metrics_pheno, p_metrics_in,
                       ncol = 2,
                       rel_heights = c(1, 1),
                       rel_widths = c(2, 1))