df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

nlme_peak <- lme(peak ~ year_new, data = df_metrics %>% filter(observ_pct >= 0.7) %>% filter(state != "PR") %>% filter(peak_check == 1), random = ~ year_new | n)
summary(nlme_peak)

nlme_peak_doy <- lme(peak_doy ~ year_new, data = df_metrics %>% filter(observ_pct >= 0.7) %>% filter(state != "PR") %>% filter(peak_check == 1), random = ~ year_new | n)
summary(nlme_peak_doy)

nlme_integral <- lme(integral ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR"), random = ~ year_new | n)
summary(nlme_integral)

nlme_sos <- lme(sos ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR"), random = ~ year_new | n)
summary(nlme_sos)

nlme_eos <- lme(eos ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR"), random = ~ year_new | n)
summary(nlme_eos)

nlme_los <- lme(los ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR"), random = ~ year_new | n, control = lmeControl(opt = "optim"))
summary(nlme_los)

# , control = lmeControl(opt = "optim")


# remove CA
nlme_peak <- lme(peak ~ year_new, data = df_metrics %>% filter(observ_pct >= 0.7) %>% filter(state != "PR") %>% filter(peak_check == 1)%>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n, control = lmeControl(opt = "optim"))
summary(nlme_peak)

nlme_peak_doy <- lme(peak_doy ~ year_new, data = df_metrics %>% filter(observ_pct >= 0.7) %>% filter(state != "PR") %>% filter(peak_check == 1)%>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n)
summary(nlme_peak_doy)

nlme_integral <- lme(integral ~ year_new, data = df_metrics %>% filter(observ_pct >= 0.7) %>% filter(state != "PR") %>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n)
summary(nlme_integral)

nlme_sos <- lme(sos ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR") %>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n, control = lmeControl(opt = "optim"))
summary(nlme_sos)

nlme_eos <- lme(eos ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR") %>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n, control = lmeControl(opt = "optim"))
summary(nlme_eos)

nlme_los <- lme(los ~ year_new, data = df_metrics %>% filter(observ_pct >= 1) %>% filter(state != "PR") %>% filter(!(n %in% c(3, 6, 17, 22, 29, 40))), random = ~ year_new | n, control = lmeControl(opt = "optim"))
summary(nlme_los)

## integral
# fit lme model and check residuals
data_integral <- df_metrics %>% 
  filter(observ_pct >= 1) %>% 
  # filter(state != "PR") %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, start_year, end_year, Nyear, Nrcd) %>% 
  mutate(intercept = lm(log(integral) ~ year_new)$coefficients[1]) %>% 
  mutate(slope = lm(log(integral) ~ year_new)$coefficients[2]) %>% 
  filter(Nrcd >= 5) %>% 
  ungroup()

lme_integral <- lme(log(integral) ~ year_new, data = data_integral, random = ~ year_new | n)
summary(lme_integral)

residuals <- residuals(lme_integral)
hist(residuals, breaks = "FD", col = "skyblue", main = "Residuals Histogram")
plot(density(residuals), main = "Residuals Density Plot")
qqnorm(residuals)
qqline(residuals)

integral_fit <- data_integral %>% 
  mutate(
    integral_fit.fixed = lme_integral$fitted[, 1],
    integral_fit.random = lme_integral$fitted[, 2]
  ) %>%
  as_tibble()
integral_fix_var <- ggpredict(lme_integral, terms = c("year_new", "n"), type = "re") %>%
  as_tibble()

# scatterplot of linear trend & overall trend in lme
ggplot(
  data = data_integral %>% mutate(n = as.character(n)),
  aes(x = year_new, y = log(integral))
) +
  geom_point(aes(color = n), size = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
  geom_path(
    data = integral_fit,
    aes(x = year_new, y = integral_fit.fixed),
    col = "black", linewidth = 1
  ) +
  ggtitle("temporal trends of annual integral (100% data completeness)")
