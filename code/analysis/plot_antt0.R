# df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_amplitude.rds"))
# df_antt <- df_smooth %>%
#   filter(n == 10) %>%
#   filter(year == 2009 | (year == 2010 & doy <= offset*2)) %>%
#   left_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset", "year_new"))
# write_rds(df_antt, str_c(.path$dat_process, "2023-04-25/dat_antt.rds"))

df_antt <- read_rds(str_c(.path$dat_process, "2023-04-25/dat_antt.rds"))
p_antt0 <- ggplot() +
  # geom_ribbon_pattern(
  #   data = df_antt %>% filter(year_new == 2009 & doy_new %in% 137:236),
  #   aes(x = date, ymin = 0, ymax = count_whit, pattern_colour = "allergy season\nintegral (ASIn)"),
  #   pattern = "stripe",
  #   pattern_fill = "dark red",
  #   fill = NA,
  #   alpha = 0.4) +
  # scale_pattern_colour_manual(
  #   name = "",
  #   values = c("dark red"),
  #   guide = guide_legend(alpha = 0.4)) +
  # geom_ribbon_pattern(
  #   data = df_antt %>% filter(year_new == 2009 & doy_new %in% 1:78),
  #   aes(x = date, ymin = 0, ymax = count_whit, pattern_fill = "10% of the\nannual integral"),
  #   pattern = "stripe",
  #   pattern_colour = "orange",
  #   fill = NA) +
  # scale_pattern_fill_manual(
  #   name = "",
  #   values = c("orange"),
  #   guide = guide_legend(alpha = 0.4)) +
  # geom_ribbon_pattern(
  #   data = df_antt %>% filter(year_new == 2009 & doy_new %in% 282:365),
  #   aes(x = date, ymin = 0, ymax = count_whit),
  #   pattern = "stripe",
  #   pattern_colour = "orange",
  #   pattern_fill = "orange",
  #   fill = NA,
  #   show.legend = F) +
  # geom_ribbon(
  #   data = df_antt %>% filter(year_new == 2009 & doy_new %in% 1:365),
  #   aes(x = date, ymin = 0, ymax = count_whit, fill = "annual integral\n(AIn)"),
  #   alpha = 0.4) +
  # scale_fill_manual(
  #   name='',
  #   values= "orange",
  #   guide = guide_legend(override.aes = list(alpha = 0.4))) +
  # geom_segment(
  #   aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-08-12"), y = 7955.968, yend = 7955.968),
  #   col = "dark red") +
  # geom_text(
  #   aes(x = as_datetime("2009-01-01"), y = 7955.968, label = "peak concentration (Cp)"),
  #   hjust = 0,
  #   vjust = -0.5,
  #   col = "dark red",
  #   size = 4) +
  # geom_segment(
  #   aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-02-11"), y = 1333.443, yend = 1333.443),
  #   col = "orange",
  #   linetype = "dashed") +
  # geom_text(
  #   aes(x = as_datetime("2009-01-01"), y = 1333.443, label = "trough concentration"),
  #   hjust = 0,
  #   vjust = -0.5,
  #   col = "orange",
  #   size = 4) +
  # geom_segment(
  #   aes(x = as_datetime("2009-02-11"), xend = as_datetime("2009-02-11"), y = 1333.443, yend = 7955.968),
  #   col = "orange") +
  # geom_text(
  #   aes(x = as_datetime("2009-02-11"), y = exp((log(1333.443) + log(7955.968)) / 2), label = "amplitude concentration (Ca)"),
  #   hjust = 0,
  #   vjust = -0.5,
  #   col = "orange",
  #   size = 4) +
  # geom_segment(
  #   data = df_antt %>% filter(doy_new == sos | doy_new == eos),
  #   aes(x = date, xend = date, y = 0, yend = count_whit),
  #   col = "orange") +
  # geom_segment(
  #   data = df_antt %>% filter(doy_new == sas | doy_new == eas),
  #   aes(x = date, xend = date, y = 0, yend = count_whit),
  #   col = "dark red") +
  # geom_segment(
  #   aes(x = as_datetime("2009-01-01"), xend = as_datetime("2009-10-04"), y = 6500, yend = 6500),
  #   linetype = "dashed",
  #   col = "dark red") +
  # geom_text(
  #   aes(x = as_datetime("2009-01-01"), y = 6500, label = "NAB allergy threshold"),
  #   hjust = 0,
  #   vjust = 1.3,
  #   col = "dark red",
  #   size = 4) +
  # geom_segment(
  #   data = df_antt %>% filter(doy_new == sas | doy_new == eas),
  #   aes(x = date, xend = date, y = count_whit, yend = 9000),
  #   linetype = "dashed",
  #   col = "dark red") +
  # geom_segment(
  #   aes(x = as_datetime("2009-06-27"), xend = as_datetime("2009-10-04"), y = 9000, yend = 9000),
  #   col = "dark red") +
  # geom_text(
  #   aes(x = as_datetime("2009-08-16"), y = 9000, label = "length of allergy season (LAS)"),
  #   hjust = 0.5,
  #   vjust = -0.3,
  #   col = "dark red",
  #   size = 4) +
  # geom_segment(
  #   data = df_antt %>% filter(doy_new == sos | doy_new == eos),
  #   aes(x = date, xend = date, y = count_whit, yend = 10500),
  #   linetype = "dashed",
  #   col = "orange") +
  # geom_segment(
  #   aes(x = as_datetime("2009-04-29"), xend = as_datetime("2009-11-19"), y = 10500, yend = 10500),
  #   col = "orange") +
  # geom_text(
  #   aes(x = as_datetime("2009-08-09"), y = 10500, label = "length of spore season (LOS)"),
  #   hjust = 0.5,
  #   vjust = -0.3,
  #   col = "orange",
  #   size = 4) +
  geom_path(
    data = df_antt %>% filter(year_new == 2009),
    aes(x = date, y = count_whit),
    col = "black") +
  geom_path(
    data = df_antt %>% filter(year_new == 2008),
    aes(x = date, y = count_whit),
    linetype = "dashed",
    col = "black") +
  geom_path(
    data = df_antt %>% filter(year_new == 2010),
    aes(x = date, y = count_whit),
    linetype = "dashed",
    col = "black") +
  # geom_point(
  #   aes(x = as_datetime("2009-08-12"), y = 7955.968),
  #   col = "dark red") +
  scale_y_continuous(
    limits = c(10^3, 20000),
    trans = "log10",
    breaks = c(10^3, 10^3.5, 10^4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # annotation_logticks(sides = "l") +
  # scale_x_datetime(
  #   breaks = c(as_datetime("2009-02-10"), as_datetime("2010-02-10")), 
  #   date_labels = c("1","365")) +
  coord_cartesian(
    xlim = c(as_datetime("2009-01-21"), max(df_antt$date))) +
  ylab(expression("Spore concentration (spores m"^-3*")")) +
  xlab("Day of spore year") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box.just = "top",
    axis.ticks.x = element_blank()) +
  theme(
    axis.text.x = element_text(color = "black", angle = 15, hjust = 1, size = 12),
    legend.text = element_text(size = 12))
