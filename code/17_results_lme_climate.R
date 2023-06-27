df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_daymet.rds"))

### peak
df_plot <- tibble(
  n = 1:4,
  a = vector("list", 4),
  b = vector("list", 4)
)
for (i in c(1:4)) {
  ## scatterplot: peak ~ tap
  # filter data
  pct = 1 - 0.1*(i - 1)
  data_peak <- df_metrics %>% 
    filter(peak_check == 1) %>%
    drop_na(peak) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    do({
      result <- lm(log(peak) ~ year_new, .)
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
    rename("slope" = "year_new", "intercept" = "(Intercept)") %>%
    ungroup() %>% 
    right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
    filter(peak_check == 1) %>%
    drop_na(peak) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    mutate(start_year = min(year_new)) %>% 
    mutate(end_year = max(year_new)) %>% 
    mutate(Nyear = end_year - start_year + 1) %>%
    mutate(Nrcd = n()) %>% 
    filter(Nrcd >= 5) %>% 
    filter(state != "PR") %>% 
    filter(country == "US") %>% 
    ungroup()
  # fit lme
  lme_climate_peak <- lme(
    log(peak) ~ mat + tap,
    data = data_peak,
    random = ~ 1 | n)
  summary(lme_climate_peak)
  peak_fit <- data_peak %>%
    mutate(
      peak_fit.fixed = lme_climate_peak$fitted[, 1],
      peak_fit.random = lme_climate_peak$fitted[, 2]
    ) %>%
    as_tibble()
  fixed_effects <- fixef(lme_climate_peak)
  peak_fit$predict <- fixed_effects["tap"] * peak_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(peak_fit$mat)
  peak_fix_var <- ggpredict(lme_climate_peak, terms = c("tap", "n"), type = "re") %>%
    as_tibble()
  p <- summary(lme_climate_peak)$tTable[["tap","p-value"]]
  # plot
  p_a <- ggplot() +
    geom_ribbon(
      data = peak_fix_var,
      aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
      col = "gray",
      alpha = 0.2
    ) +
    geom_point(
      data = data_peak %>% mutate(n = as.character(n)),
      aes(x = tap, y = log(peak), color = n),
      size = 0.5
    ) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_smooth(
      data = data_peak %>% mutate(n = as.character(n)),
      method = "lm",
      se = FALSE,
      aes(x = tap, y = log(peak), group = n, col = n),
      linewidth = 0.5
    ) +
    geom_line(
      data = peak_fit,
      aes(x = tap, y = predict),
      color = "red",
      linewidth = 1
    ) +
    xlab("tap") +
    ggtitle(paste0(
      pct*100, "% data completeness",
      "\nslope = ", round(fixed_effects["tap"], digits = 6),
      "\np-value = ", round(p, digits = 6)))
  ## scatterplot: trend
  # filter data
  data_peak_slope <- data_peak %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
    do({
      result <- lm(tap ~ year_new, .)
      data_frame(
        r_squared_tap =
          result %>%
          summary() %>%
          magrittr::use_series(adj.r.squared),
        p_value_tap =
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
    rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
    ungroup()
  # fit lm
  lm_slope_peak <- lm(
    slope ~ slope_tap,
    data = data_peak_slope)
  summary(lm_slope_peak)
  predictions <- predict(lm_slope_peak, interval = "confidence", level = 0.95)
  slope_var <- data_peak_slope %>% 
    mutate(
      ymin = predictions[, "lwr"],
      ymax = predictions[, "upr"]
    )
  p_b <- ggplot(data = slope_var, aes(x = slope_tap, y = slope)) +
    geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
    geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
    geom_point(size = 1) +
    theme_classic() +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
    xlab("trend of tap") +
    ylab("trend of log(peak)")+
    ggtitle(paste0(
      "slope = ", round(summary(lm_slope_peak)$coefficient[["slope_tap", "Estimate"]], digits = 6),
      "\nr-squared = ", round(summary(lm_slope_peak)$r.squared, digits = 6),
      "\np-value = ", round(summary(lm_slope_peak)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b
}
pdf(
  "output/figures/p_scatter_log(peak)_tap.pdf",
  width = 16,
  height = 6
)
for (i in 1:4) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()

### peak date
df_plot <- tibble(
  n = 1:4,
  a = vector("list", 4),
  b = vector("list", 4)
)
for (i in c(1:4)) {
  ## scatterplot: peak date ~ tap
  # filter data
  pct = 1 - 0.1*(i - 1)
  data_peak_doy <- df_metrics %>% 
    filter(peak_check == 1) %>%
    drop_na(peak_doy) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    do({
      result <- lm(peak_doy ~ year_new, .)
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
    rename("slope" = "year_new", "intercept" = "(Intercept)") %>%
    ungroup() %>% 
    right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
    filter(peak_check == 1) %>%
    drop_na(peak_doy) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    mutate(start_year = min(year_new)) %>% 
    mutate(end_year = max(year_new)) %>% 
    mutate(Nyear = end_year - start_year + 1) %>%
    mutate(Nrcd = n()) %>% 
    filter(Nrcd >= 5) %>% 
    filter(state != "PR") %>% 
    filter(country == "US") %>% 
    ungroup()
  # fit lme
  lme_climate_peak_doy <- lme(
    peak_doy ~ mat + tap,
    data = data_peak_doy,
    random = ~ 1 | n)
  summary(lme_climate_peak_doy)
  peak_doy_fit <- data_peak_doy %>%
    mutate(
      peak_doy_fit.fixed = lme_climate_peak_doy$fitted[, 1],
      peak_doy_fit.random = lme_climate_peak_doy$fitted[, 2]
    ) %>%
    as_tibble()
  fixed_effects <- fixef(lme_climate_peak_doy)
  peak_doy_fit$predict <- fixed_effects["tap"] * peak_doy_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(peak_doy_fit$mat)
  peak_doy_fix_var <- ggpredict(lme_climate_peak_doy, terms = c("tap", "n"), type = "re") %>%
    as_tibble()
  p <- summary(lme_climate_peak_doy)$tTable[["tap","p-value"]]
  # plot
  p_a <- ggplot() +
    geom_point(
      data = data_peak_doy %>% mutate(n = as.character(n)),
      aes(x = tap, y = peak_doy, color = n),
      size = 0.5
    ) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_smooth(
      data = data_peak_doy %>% mutate(n = as.character(n)),
      method = "lm",
      se = FALSE,
      aes(x = tap, y = peak_doy, group = n, col = n),
      linewidth = 0.5
    ) +
    geom_ribbon(
      data = peak_doy_fix_var,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      col = "gray",
      alpha = 0.2
    ) +
    geom_line(
      data = peak_doy_fit,
      aes(x = tap, y = predict),
      color = "red",
      linewidth = 1
    ) +
    xlab("tap") +
    ggtitle(paste0(
      pct*100, "% data completeness",
      "\nslope = ", round(fixed_effects["tap"], digits = 6),
      "\np-value = ", round(p, digits = 6)))
  ## scatterplot: trend
  # filter data
  data_peak_doy_slope <- data_peak_doy %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
    do({
      result <- lm(tap ~ year_new, .)
      data_frame(
        r_squared_tap =
          result %>%
          summary() %>%
          magrittr::use_series(adj.r.squared),
        p_value_tap =
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
    rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
    ungroup()
  # fit lm
  lm_slope_peak_doy <- lm(
    slope ~ slope_tap,
    data = data_peak_doy_slope)
  summary(lm_slope_peak_doy)
  predictions <- predict(lm_slope_peak_doy, interval = "confidence", level = 0.95)
  slope_var <- data_peak_doy_slope %>% 
    mutate(
      ymin = predictions[, "lwr"],
      ymax = predictions[, "upr"]
    )
  p_b <- ggplot(data = slope_var, aes(x = slope_tap, y = slope)) +
    geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
    geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
    geom_point(size = 1) +
    theme_classic() +
    geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
    xlab("trend of tap") +
    ylab("trend of peak date")+
    ggtitle(paste0(
      "slope = ", round(summary(lm_slope_peak_doy)$coefficient[["slope_tap", "Estimate"]], digits = 6),
      "\nr-squared = ", round(summary(lm_slope_peak_doy)$r.squared, digits = 6),
      "\np-value = ", round(summary(lm_slope_peak_doy)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b
}
pdf(
  "output/figures/p_scatter_peak_doy_tap.pdf",
  width = 16,
  height = 6
)
for (i in 1:4) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()

### annual integral
df_plot <- tibble(
  n = 1:4,
  a = vector("list", 4),
  b = vector("list", 4)
)
for (i in c(1:4)) {
  ## scatterplot: log(integral) ~ tap
  # filter data
  pct = 1 - 0.1*(i - 1)
  data_integral <- df_metrics %>% 
    drop_na(integral) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    do({
      result <- lm(log(integral) ~ year_new, .)
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
    rename("slope" = "year_new", "intercept" = "(Intercept)") %>%
    ungroup() %>% 
    right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
    drop_na(integral) %>% 
    filter(observ_pct >= pct) %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
    mutate(start_year = min(year_new)) %>% 
    mutate(end_year = max(year_new)) %>% 
    mutate(Nyear = end_year - start_year + 1) %>%
    mutate(Nrcd = n()) %>% 
    filter(Nrcd >= 5) %>% 
    filter(state != "PR") %>% 
    filter(country == "US") %>% 
    ungroup()
  # fit lme
  lme_climate_integral <- lme(
    log(integral) ~ mat + tap,
    data = data_integral,
    random = ~ 1 | n)
  summary(lme_climate_integral)
  integral_fit <- data_integral %>%
    mutate(
      integral_fit.fixed = lme_climate_integral$fitted[, 1],
      integral_fit.random = lme_climate_integral$fitted[, 2]
    ) %>%
    as_tibble()
  fixed_effects <- fixef(lme_climate_integral)
  integral_fit$predict <- fixed_effects["tap"] * integral_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(integral_fit$mat)
  integral_fix_var <- ggpredict(lme_climate_integral, terms = c("tap", "n"), type = "re") %>%
    as_tibble()
  p <- summary(lme_climate_integral)$tTable[["tap","p-value"]]
  # plot
  p_a <- ggplot() +
    geom_ribbon(
      data = integral_fix_var,
      aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
      col = "gray",
      alpha = 0.2
    ) +
    geom_point(
      data = data_integral %>% mutate(n = as.character(n)),
      aes(x = tap, y = log(integral), color = n),
      size = 0.5
    ) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_smooth(
      data = data_integral %>% mutate(n = as.character(n)),
      method = "lm",
      se = FALSE,
      aes(x = tap, y = log(integral), group = n, col = n),
      linewidth = 0.5
    ) +
    geom_line(
      data = integral_fit,
      aes(x = tap, y = predict),
      color = "red",
      linewidth = 1
    ) +
    xlab("tap") +
    ggtitle(paste0(
      pct*100, "% data completeness",
      "\nslope = ", round(fixed_effects["tap"], digits = 6),
      "\np-value = ", round(p, digits = 6)))
  ## scatterplot: trend
  # filter data
  data_integral_slope <- data_integral %>% 
    group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
    do({
      result <- lm(tap ~ year_new, .)
      data_frame(
        r_squared_tap =
          result %>%
          summary() %>%
          magrittr::use_series(adj.r.squared),
        p_value_tap =
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
    rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
    ungroup()
  # fit lm
  lm_slope_integral <- lm(
    slope ~ slope_tap,
    data = data_integral_slope)
  summary(lm_slope_integral)
  predictions <- predict(lm_slope_integral, interval = "confidence", level = 0.95)
  slope_var <- data_integral_slope %>% 
    mutate(
      ymin = predictions[, "lwr"],
      ymax = predictions[, "upr"]
    )
  p_b <- ggplot(data = slope_var, aes(x = slope_tap, y = slope)) +
    geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
    geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
    geom_point(size = 1) +
    theme_classic() +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
    xlab("trend of tap") +
    ylab("trend of log(integral)")+
    ggtitle(paste0(
      "slope = ", round(summary(lm_slope_integral)$coefficient[["slope_tap", "Estimate"]], digits = 6),
      "\nr-squared = ", round(summary(lm_slope_integral)$r.squared, digits = 6),
      "\np-value = ", round(summary(lm_slope_integral)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b
}
pdf(
  "output/figures/p_scatter_log(integral)_tap.pdf",
  width = 16,
  height = 6
)
for (i in 1:4) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()




## sos
data_sos <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sos %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_climate_sos <- lme(
  los ~ mat + tap,
  data = data_sos,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_climate_sos)





## sas
data_sas <- df_metrics %>% 
  drop_na(eas) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_sas %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_sas <- lme(
  eas ~ mat + tap,
  data = data_sas,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_sas)
## las
data_las <- df_metrics %>% 
  drop_na(las) %>% 
  filter(observ_pct_as >= 0.7) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>% 
  ungroup()
# view(data_las %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))

# fit lme
lme_las <- lme(
  las ~ mat + tap,
  data = data_las,
  random = ~ 1 | n
  # ,
  # control = lmeControl(opt = "optim")
)
summary(lme_las)





## as integral
data_integral_as <- df_metrics %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= 0.8) %>%
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(log(integral_as) ~ year_new, .)
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
  rename("slope" = "year_new", "intercept" = "(Intercept)") %>%
  ungroup() %>% 
  right_join(df_metrics, by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "offset")) %>% 
  drop_na(integral_as) %>% 
  filter(observ_pct_as >= 0.8) %>%
  group_by(lat, lon, station, city, state, country, id, n) %>% 
  mutate(start_year = min(year_new)) %>% 
  mutate(end_year = max(year_new)) %>% 
  mutate(Nyear = end_year - start_year + 1) %>%
  mutate(Nrcd = n()) %>% 
  filter(Nrcd >= 5) %>% 
  filter(state != "PR") %>% 
  filter(country == "US") %>%
  ungroup() %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
  do({
    result <- lm(tap ~ year_new, .)
    data_frame(
      r_squared_tap =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_tap =
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
  rename("slope_tap" = "year_new", "intercept_tap" = "(Intercept)") %>%
  ungroup()
# view(data_integral_as %>% distinct(n, .keep_all = T) %>% dplyr::select(lat, lon, city, state, n))
lm_integral_as <- lm(slope ~ slope_tap, data = data_integral_as)
summary(lm_integral_as)
p3 <- ggplot(
  data = data_integral_as,
  aes(x = slope_tap, y = slope)
) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1)
pdf(
  "output/figures/p_scatter_trend_log(integral_as)_tap.pdf",
  width = 8,
  height = 8 * .618
)
p1
p2
p3
dev.off()

# # fit lme
# lme_integral_as <- lme(
#   log(integral_as) ~ mat + tap,
#   data = data_integral_as,
#   random = ~ 1 | n
#   # ,
#   # control = lmeControl(opt = "optim")
# )
# summary(lme_integral_as)
# integral_fit <- data_integral_as %>% 
#   mutate(
#     integral_fit.fixed = lme_integral_as$fitted[, 1],
#     integral_fit.random = lme_integral_as$fitted[, 2]
#   ) %>%
#   as_tibble()
# fixed_effects <- fixef(lme_integral_as)
# integral_fit$predict <- fixed_effects["tap"] * integral_fit$tap + fixed_effects["(Intercept)"]
# 
# p3 <- ggplot(
#   data = data_integral_as %>% mutate(n = as.character(n)),
#   aes(x = tap, y = log(integral))
# ) +
#   geom_point(aes(color = n), size = 0.5) +
#   theme_classic() +
#   # theme(legend.position = "none") +
#   # geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = n, col = n), linewidth = 0.5) +
#   geom_line(data = integral_fit, aes(y = predict), color = "red", linewidth = 1)
# # geom_path(
# #   data = peak_fit,
# #   aes(x = tap, y = peak_fit.fixed),
# #   col = "black", linewidth = 1
# # )
# pdf(
#   "output/figures/p_scatter_log(integral as)_tap.pdf",
#   width = 12,
#   height = 6
# )
# p1
# p2
# p3
# dev.off()