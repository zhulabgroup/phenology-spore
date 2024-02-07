# source("code/analysis/calc_anom.R")
# source("code/analysis/calc_lm_anom.R")
# source("code/analysis/calc_lme_anom.R")
# source("code/analysis/plot_lme_anom.R")
pct = 0.8

plot_lme_climate <- function(metric, x_lab) {
  df_anom <- calc_anom(df_raw = df_ana, metric = metric, pct = 0.8)
  df_lm <- calc_lm_anom(df_in = df_anom, metric = metric, pct = 0.8, x = x_lab)
  m_lme <- calc_lme_anom(df_in = df_lm)
  p <- plot_lme_anom(df_in = df_lm, m = m_lme, x_lab = x_lab, y_lab = metric, lmd = lmd)
  
  return(p)
}

p_a <- plot_lme_climate(metric = "SOS", x_lab = "Anomaly of MAT") + labs(title = expression(paste(bold("A      "), italic("warmer, earlier SOS"))))
p_b <- plot_lme_climate(metric = "SAS", x_lab = "Anomaly of MAT") + labs(title = expression(paste(bold("B      "), italic("warmer, earlier SOS"))))
p_c <- plot_lme_climate(metric = "SOS", x_lab = "Anomaly of TAP") + labs(title = expression(paste(bold("C      "), italic("wetter, later SOS"))))
p_d <- plot_lme_climate(metric = "SAS", x_lab = "Anomaly of TAP") + labs(title = expression(paste(bold("D      "), italic("wetter, earlier SOS"))))
p_e <- plot_lme_climate(metric = "LOS", x_lab = "Anomaly of TAP") + labs(title = expression(paste(bold("E      "), italic("wetter, shorter LOS"))))
p_f <- plot_lme_climate(metric = "LAS", x_lab = "Anomaly of TAP") + labs(title = expression(paste(bold("F      "), italic("wetter, longer LAS"))))
p_h <- plot_lme_climate(metric = "ln_AIn", x_lab = "Anomaly of TAP") + labs(title = expression(paste(bold("H      "), italic("wetter, larger AIn"))))



# source("code/analysis/calc_trend_station.R")
df_trend_ain <- calc_trend_station(df_in = df_ana, metric = "ln_AIn", pct = 0.8)
df_trend_ain <- df_trend_ain%>% 
  mutate(rescaled_slope = sign(slope) * abs(slope)^(1/3))
p_g <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(
    data = df_trend_ain, 
    aes(x = lon, y = lat, size = Nyear, color = rescaled_slope),
    alpha = 0.8) +
  scale_size_continuous(
    range = c(5, 7),
    breaks = round(seq(min(df_trend_ain$Nyear), max(df_trend_ain$Nyear), length.out = 5)),
    name = "Number of year") +
  scale_color_gradient2(
    low = "blue", mid= "white", high = "red", midpoint = 0,
    breaks = c(-0.6, -0.3, 0, 0.2, 0.4),
    labels = function(x) {ifelse(x == 0, "0", paste0(x, "\u00B3"))},
    name = "Temporal trend of\nln(annual integral)") +
  geom_point(data = df_trend_ain, aes(x = lon, y = lat, size = Nyear, shape = ifelse(p_value > 0.05, "> 0.05", "<= 0.05"))) +
  scale_shape_manual(values = c(10, 1), guide = guide_legend(title = "P-value", override.aes = list(size = c(4, 4)))) +
  ggtitle("G") +
  guides(
    size = "none",
    shape = "none") +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 0, 0),
        plot.title = element_text(face = "bold"),
        legend.position = "left")

p_r12 <- plot_grid(p_a, p_c, p_e,
                   p_b, p_d, p_f,
                   nrow = 2,
                   rel_widths = c(1, 1, 1),
                   rel_heights = c(1, 1))
p_r3 <- plot_grid(p_g, p_h,
                  nrow = 1,
                  rel_widths = c(2, 1))
p_climate <- plot_grid(p_r12, p_r3,
                       nrow = 2,
                       rel_heights = c(2, 1))
