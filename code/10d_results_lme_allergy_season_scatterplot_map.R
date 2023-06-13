df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

## peak

# filter data
data_peak <- df_metrics %>% 
  drop_na(peak) %>% 
  filter(peak_check == 1) %>%
  filter(observ_pct >= 0.5) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(log(peak) ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(log(peak) ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_peak <- lme(
  # peak ~ year_new,
  log(peak) ~ year_new,
  data = data_peak
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
  )
summary(lme_peak)

# check residuals
df_residuals <- residuals(lme_peak) %>% 
  data.frame() %>% 
  rename("residual" = ".")
p23 <- ggplot(df_residuals, aes(x = residual)) +
  geom_density(color = "black") +
  labs(x = "Residuals", y = "Density") +
  ggtitle(paste0("data completeness: 70%", "\nlog(peak) ~ year")) +
  theme_classic()
p24 <- ggplot(df_residuals, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle(paste0("slope = -0.004749 ", "\np-value = 0.3549")) +
  # ggtitle("QQ Plot of Residuals") +
  theme_classic()
pdf(
  "output/figures/p_residuals_peak.pdf",
  width = 8,
  height = 8*0.618
)
grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
grid.arrange(p7, p9, p11, p8, p10, p12, ncol = 3)
grid.arrange(p13, p15, p17, p14, p16, p18, ncol = 3)
grid.arrange(p19, p21, p23, p20, p22, p24, ncol = 3)
dev.off()

# map & scatterplot



## peak date
data_peak_doy <- df_metrics %>% 
  drop_na(peak) %>% 
  filter(peak_check == 1) %>%
  filter(observ_pct >= 0.6) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(peak_doy ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(peak_doy ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_peak_doy <- lme(
  peak_doy ~ year_new,
  data = data_peak_doy
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_peak_doy)



## annual integral
data_integral <- df_metrics %>% 
  drop_na(integral) %>% 
  filter(observ_pct >= 0.7) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(integral ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(integral ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_integral <- lme(
  # integral ~ year_new,
  log(integral) ~ year_new,
  data = data_integral
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_integral)

# check residuals
df_residuals <- residuals(lme_integral) %>% 
  data.frame() %>% 
  rename("residual" = ".")
p23 <- ggplot(df_residuals, aes(x = residual)) +
  geom_density(color = "black") +
  labs(x = "Residuals", y = "Density") +
  ggtitle(paste0("data completeness: 70%", "\nlog(integral) ~ year")) +
  theme_classic()
p24 <- ggplot(df_residuals, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle(paste0("slope = -0.00930", "\np-value = 0.0594")) +
  # ggtitle("QQ Plot of Residuals") +
  theme_classic()
pdf(
  "output/figures/p_residuals_integral.pdf",
  width = 10,
  height = 10*0.618
)
grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
grid.arrange(p7, p9, p11, p8, p10, p12, ncol = 3)
grid.arrange(p13, p15, p17, p14, p16, p18, ncol = 3)
grid.arrange(p19, p21, p23, p20, p22, p24, ncol = 3)
dev.off()



## sos
data_sos <- df_metrics %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(sos ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(sos ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_sos <- lme(
  sos ~ year_new,
  data = data_sos
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sos)



## eos
data_eos <- df_metrics %>% 
  drop_na(eos) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(eos ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(eos ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_eos <- lme(
  eos ~ year_new,
  data = data_eos
  # %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_eos)



## los
data_los <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  # mutate(intercept = lm(los ~ year_new)$coefficients[1]) %>% 
  # mutate(slope = lm(los ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

# fit lme
lme_los <- lme(
  los ~ year_new,
  data = data_los
  %>% filter(state != "PR")
  ,
  random = ~ year_new | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_los)


