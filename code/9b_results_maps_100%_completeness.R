df_smooth <- read_rds(str_c(.path$dat_process, "2023-04-25/fill_smooth_offset.rds"))
df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

# integral map
data_integral <- df_metrics %>%
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>%
  mutate(Nyear = max(year_new) - min(year_new) + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 3) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, Nyear, Nrcd) %>%
  do({
    result <- lm(integral ~ year_new, .)
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
  rename("slope" = "year_new", "intercept" = "(Intercept)")
p3 <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  coord_map("conic", lat0 = 30) +
  theme_void() +
  geom_point(data = data_integral %>% filter(slope > 0), aes(x = lon, y = lat, size = Nyear, color = log(slope)), alpha = 0.5) +
  geom_point(data = data_integral %>% filter(slope < 0), aes(x = lon, y = lat, size = Nyear, color = -log(abs(slope))), alpha = 0.5) +
  scale_size_continuous(range = c(3, 6)) +
  scale_color_gradient2(low = "blue", mid= "white", high = "red", midpoint = 0) +
  geom_point(data = data_integral %>% filter(p_value > 0.05), aes(x = lon, y = lat, size = Nyear), shape = 1, color = "black") +
  geom_point(data = data_integral %>% filter(p_value <= 0.05), aes(x = lon, y = lat, size = Nyear), shape =10, color = "black") +
  ggtitle("temporal trends of annual integral (>=80% data completeness, 8/26 stations)")
pdf(
  "output/figures/p_map_integral_flags.pdf",
  width = 8,
  height = 8 * .618
)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

# integral scatter plot
ggplot(
  data = df_metrics %>% filter(observ_pct >= 1),
  aes(x = year_new, y = integral)
  ) +
  geom_point(aes(color = n)) +
  # guides(color = FALSE) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  # scale_y_continuous(
  #   trans = scales::log_trans(),
  #   breaks = scales::trans_breaks("log", function(x) exp(x)),
  #   labels = scales::trans_format("log", scales::math_format(e^.x))
  # ) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("temporal trends of annual integral (90% data completeness)")

ggplot() +
  geom_jitter(data = integral_fit, aes(x = year, y = integral, group = location, col = location), alpha = 0.5) +
  geom_path(data = integral_fit, aes(x = year, y = integral_fit.random, group = location, col = location), alpha = 0.5) +
  geom_path(data = integral_fit, aes(x = year, y = integral_fit.fixed), col = "black", linewidth = 1.5) +
  geom_ribbon(data = integral_fix_var, aes(x = x, ymin = conf.low, ymax = conf.high), col = "gray", alpha = 0.2) +
  scale_x_continuous(breaks = seq(2007, 2021, by = 3)) +
  theme_classic() +
  ylab("Seasonal Integral in Apr-Oct") +
  xlab("Year") +
  theme(legend.position = "none") +
  # scale_y_sqrt()
  scale_y_continuous(labels = function(x) x/1e+7) +
  ylab(expression(paste("Seasonal Integral in Apr-Oct (*10"^7*")")))