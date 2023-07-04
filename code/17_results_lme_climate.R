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
  # fit lme
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



### season
df_plot <- tibble(
  n = 1:3,
  a = vector("list", 3),
  b = vector("list", 3)
)
## scatterplot: sos ~ tap
# filter data
data_sos <- df_metrics %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(sos ~ year_new, .)
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
  drop_na(sos) %>% 
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
# fit lme
lme_climate_sos <- lme(
  sos ~ mat + tap,
  data = data_sos,
  random = ~ 1 | n)
summary(lme_climate_sos)
sos_fit <- data_sos %>%
  mutate(
    sos_fit.fixed = lme_climate_sos$fitted[, 1],
    sos_fit.random = lme_climate_sos$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_sos)
sos_fit$predict <- fixed_effects["tap"] * sos_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(sos_fit$mat)
sos_fix_var <- ggpredict(lme_climate_sos, terms = c("tap", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_sos)$tTable[["tap","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_sos %>% mutate(n = as.character(n)),
    aes(x = tap, y = sos, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_sos %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = tap, y = sos, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = sos_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = sos_fit,
    aes(x = tap, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("tap") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["tap"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_sos_slope <- data_sos %>% 
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
lm_slope_sos <- lm(
  slope ~ slope_tap,
  data = data_sos_slope)
summary(lm_slope_sos)
predictions <- predict(lm_slope_sos, interval = "confidence", level = 0.95)
slope_var <- data_sos_slope %>% 
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
  ylab("trend of sos")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_sos)$coefficient[["slope_tap", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_sos)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_sos)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[1]] <- p_a
df_plot$b[[1]] <- p_b
## scatterplot: eos ~ tap
# filter data
data_eos <- df_metrics %>% 
  drop_na(eos) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(eos ~ year_new, .)
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
  drop_na(eos) %>% 
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
# fit lme
lme_climate_eos <- lme(
  eos ~ mat + tap,
  data = data_eos,
  random = ~ 1 | n)
summary(lme_climate_eos)
eos_fit <- data_eos %>%
  mutate(
    eos_fit.fixed = lme_climate_eos$fitted[, 1],
    eos_fit.random = lme_climate_eos$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_eos)
eos_fit$predict <- fixed_effects["tap"] * eos_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(eos_fit$mat)
eos_fix_var <- ggpredict(lme_climate_eos, terms = c("tap", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_eos)$tTable[["tap","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_eos %>% mutate(n = as.character(n)),
    aes(x = tap, y = eos, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_eos %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = tap, y = eos, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = eos_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = eos_fit,
    aes(x = tap, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("tap") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["tap"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_eos_slope <- data_eos %>% 
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
lm_slope_eos <- lm(
  slope ~ slope_tap,
  data = data_eos_slope)
summary(lm_slope_eos)
predictions <- predict(lm_slope_eos, interval = "confidence", level = 0.95)
slope_var <- data_eos_slope %>% 
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
  ylab("trend of eos")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_eos)$coefficient[["slope_tap", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_eos)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_eos)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[2]] <- p_a
df_plot$b[[2]] <- p_b
## scatterplot: los ~ tap
# filter data
data_los <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(los ~ year_new, .)
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
# fit lme
lme_climate_los <- lme(
  los ~ mat + tap,
  data = data_los,
  random = ~ 1 | n)
summary(lme_climate_los)
los_fit <- data_los %>%
  mutate(
    los_fit.fixed = lme_climate_los$fitted[, 1],
    los_fit.random = lme_climate_los$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_los)
los_fit$predict <- fixed_effects["tap"] * los_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(los_fit$mat)
los_fix_var <- ggpredict(lme_climate_los, terms = c("tap", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_los)$tTable[["tap","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_los %>% mutate(n = as.character(n)),
    aes(x = tap, y = los, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_los %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = tap, y = los, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = los_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = los_fit,
    aes(x = tap, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("tap") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["tap"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_los_slope <- data_los %>% 
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
lm_slope_los <- lm(
  slope ~ slope_tap,
  data = data_los_slope)
summary(lm_slope_los)
predictions <- predict(lm_slope_los, interval = "confidence", level = 0.95)
slope_var <- data_los_slope %>% 
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
  ylab("trend of los")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_los)$coefficient[["slope_tap", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_los)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_los)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[3]] <- p_a
df_plot$b[[3]] <- p_b
pdf(
  "output/figures/p_scatter_season_tap.pdf",
  width = 16,
  height = 6
)
for (i in 1:3) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()


## mat
df_plot <- tibble(
  n = 1:3,
  a = vector("list", 3),
  b = vector("list", 3)
)
## scatterplot: sos ~ mat
# filter data
data_sos <- df_metrics %>% 
  drop_na(sos) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(sos ~ year_new, .)
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
  drop_na(sos) %>% 
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
# fit lme
lme_climate_sos <- lme(
  sos ~ mat + tap,
  data = data_sos,
  random = ~ 1 | n)
summary(lme_climate_sos)
sos_fit <- data_sos %>%
  mutate(
    sos_fit.fixed = lme_climate_sos$fitted[, 1],
    sos_fit.random = lme_climate_sos$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_sos)
sos_fit$predict <- fixed_effects["mat"] * sos_fit$mat + fixed_effects["(Intercept)"] + fixed_effects["tap"] * mean(sos_fit$tap)
sos_fix_var <- ggpredict(lme_climate_sos, terms = c("mat", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_sos)$tTable[["mat","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_sos %>% mutate(n = as.character(n)),
    aes(x = mat, y = sos, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_sos %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = mat, y = sos, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = sos_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = sos_fit,
    aes(x = mat, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("mat") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["mat"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_sos_slope <- data_sos %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
  do({
    result <- lm(mat ~ year_new, .)
    data_frame(
      r_squared_mat =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_mat =
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
  rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
  ungroup()
# fit lm
lm_slope_sos <- lm(
  slope ~ slope_mat,
  data = data_sos_slope)
summary(lm_slope_sos)
predictions <- predict(lm_slope_sos, interval = "confidence", level = 0.95)
slope_var <- data_sos_slope %>% 
  mutate(
    ymin = predictions[, "lwr"],
    ymax = predictions[, "upr"]
  )
p_b <- ggplot(data = slope_var, aes(x = slope_mat, y = slope)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
  xlab("trend of mat") +
  ylab("trend of sos")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_sos)$coefficient[["slope_mat", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_sos)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_sos)$coefficient[["slope_mat", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[1]] <- p_a
df_plot$b[[1]] <- p_b


## scatterplot: eos ~ tap
# filter data
data_eos <- df_metrics %>% 
  drop_na(eos) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(eos ~ year_new, .)
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
  drop_na(eos) %>% 
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
# fit lme
lme_climate_eos <- lme(
  eos ~ mat + tap,
  data = data_eos,
  random = ~ 1 | n)
summary(lme_climate_eos)
eos_fit <- data_eos %>%
  mutate(
    eos_fit.fixed = lme_climate_eos$fitted[, 1],
    eos_fit.random = lme_climate_eos$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_eos)
eos_fit$predict <- fixed_effects["mat"] * eos_fit$mat + fixed_effects["(Intercept)"] + fixed_effects["tap"] * mean(eos_fit$tap)
eos_fix_var <- ggpredict(lme_climate_eos, terms = c("mat", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_eos)$tTable[["mat","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_eos %>% mutate(n = as.character(n)),
    aes(x = mat, y = eos, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_eos %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = mat, y = eos, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = eos_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = eos_fit,
    aes(x = mat, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("mat") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["mat"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_eos_slope <- data_eos %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
  do({
    result <- lm(mat ~ year_new, .)
    data_frame(
      r_squared_mat =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_mat =
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
  rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
  ungroup()
# fit lm
lm_slope_eos <- lm(
  slope ~ slope_mat,
  data = data_eos_slope)
summary(lm_slope_eos)
predictions <- predict(lm_slope_eos, interval = "confidence", level = 0.95)
slope_var <- data_eos_slope %>% 
  mutate(
    ymin = predictions[, "lwr"],
    ymax = predictions[, "upr"]
  )
p_b <- ggplot(data = slope_var, aes(x = slope_mat, y = slope)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
  xlab("trend of mat") +
  ylab("trend of eos")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_eos)$coefficient[["slope_mat", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_eos)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_eos)$coefficient[["slope_mat", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[2]] <- p_a
df_plot$b[[2]] <- p_b


## scatterplot: los ~ tap
# filter data
data_los <- df_metrics %>% 
  drop_na(los) %>% 
  filter(observ_pct >= 1) %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset) %>% 
  do({
    result <- lm(los ~ year_new, .)
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
# fit lme
lme_climate_los <- lme(
  los ~ mat + tap,
  data = data_los,
  random = ~ 1 | n)
summary(lme_climate_los)
los_fit <- data_los %>%
  mutate(
    los_fit.fixed = lme_climate_los$fitted[, 1],
    los_fit.random = lme_climate_los$fitted[, 2]
  ) %>%
  as_tibble()
fixed_effects <- fixef(lme_climate_los)
los_fit$predict <- fixed_effects["mat"] * los_fit$mat + fixed_effects["(Intercept)"] + fixed_effects["tap"] * mean(los_fit$tap)
los_fix_var <- ggpredict(lme_climate_los, terms = c("mat", "n"), type = "re") %>%
  as_tibble()
p <- summary(lme_climate_los)$tTable[["mat","p-value"]]
# plot
p_a <- ggplot() +
  geom_point(
    data = data_los %>% mutate(n = as.character(n)),
    aes(x = mat, y = los, color = n),
    size = 0.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(
    data = data_los %>% mutate(n = as.character(n)),
    method = "lm",
    se = FALSE,
    aes(x = mat, y = los, group = n, col = n),
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = los_fix_var,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    col = "gray",
    alpha = 0.2
  ) +
  geom_line(
    data = los_fit,
    aes(x = mat, y = predict),
    color = "red",
    linewidth = 1
  ) +
  xlab("mat") +
  ggtitle(paste0(
    100, "% data completeness",
    "\nslope = ", round(fixed_effects["mat"], digits = 6),
    "\np-value = ", round(p, digits = 6)))
## scatterplot: trend
# filter data
data_los_slope <- data_los %>% 
  group_by(lat, lon, station, city, state, country, id, n, offset, r_squared, p_value, intercept, slope) %>% 
  do({
    result <- lm(mat ~ year_new, .)
    data_frame(
      r_squared_mat =
        result %>%
        summary() %>%
        magrittr::use_series(adj.r.squared),
      p_value_mat =
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
  rename("slope_mat" = "year_new", "intercept_mat" = "(Intercept)") %>%
  ungroup()
# fit lm
lm_slope_los <- lm(
  slope ~ slope_mat,
  data = data_los_slope)
summary(lm_slope_los)
predictions <- predict(lm_slope_los, interval = "confidence", level = 0.95)
slope_var <- data_los_slope %>% 
  mutate(
    ymin = predictions[, "lwr"],
    ymax = predictions[, "upr"]
  )
p_b <- ggplot(data = slope_var, aes(x = slope_mat, y = slope)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "gray") +
  geom_point(size = 1) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, col = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), col = "gray", alpha = 0.2) +
  xlab("trend of mat") +
  ylab("trend of los")+
  ggtitle(paste0(
    "slope = ", round(summary(lm_slope_los)$coefficient[["slope_mat", "Estimate"]], digits = 6),
    "\nr-squared = ", round(summary(lm_slope_los)$r.squared, digits = 6),
    "\np-value = ", round(summary(lm_slope_los)$coefficient[["slope_mat", "Pr(>|t|)"]], digits = 6)))
df_plot$a[[3]] <- p_a
df_plot$b[[3]] <- p_b


pdf(
  "output/figures/p_scatter_season_mat.pdf",
  width = 16,
  height = 6
)
for (i in 1:3) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()




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





### allergy season integral
df_plot <- tibble(
  n = 1:3,
  a = vector("list", 3),
  b = vector("list", 3)
)
for (i in c(1:3)) {
  ## scatterplot: log(integral_as) ~ tap
  # filter data
  pct = 1 - 0.1*(i - 1)
  data_integral_as <- df_metrics %>% 
    drop_na(integral_as) %>% 
    filter(observ_pct_as >= pct) %>% 
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
    filter(observ_pct_as >= pct) %>% 
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
  lme_climate_integral_as <- lme(
    log(integral_as) ~ mat + tap,
    data = data_integral_as,
    random = ~ 1 | n)
  summary(lme_climate_integral_as)
  integral_as_fit <- data_integral_as %>%
    mutate(
      integral_as_fit.fixed = lme_climate_integral_as$fitted[, 1],
      integral_as_fit.random = lme_climate_integral_as$fitted[, 2]
    ) %>%
    as_tibble()
  fixed_effects <- fixef(lme_climate_integral_as)
  integral_as_fit$predict <- fixed_effects["tap"] * integral_as_fit$tap + fixed_effects["(Intercept)"] + fixed_effects["mat"] * mean(integral_as_fit$mat)
  integral_as_fix_var <- ggpredict(lme_climate_integral_as, terms = c("tap", "n"), type = "re") %>%
    as_tibble()
  p <- summary(lme_climate_integral_as)$tTable[["tap","p-value"]]
  # plot
  p_a <- ggplot() +
    geom_ribbon(
      data = integral_as_fix_var,
      aes(x = x, ymin = log(conf.low), ymax = log(conf.high)),
      col = "gray",
      alpha = 0.2
    ) +
    geom_point(
      data = data_integral_as %>% mutate(n = as.character(n)),
      aes(x = tap, y = log(integral_as), color = n),
      size = 0.5
    ) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_smooth(
      data = data_integral_as %>% mutate(n = as.character(n)),
      method = "lm",
      se = FALSE,
      aes(x = tap, y = log(integral_as), group = n, col = n),
      linewidth = 0.5
    ) +
    geom_line(
      data = integral_as_fit,
      aes(x = tap, y = predict),
      color = "red",
      linewidth = 1
    ) +
    xlab("tap") +
    ggtitle(paste0(
      pct*100, "% data completeness in as",
      "\nslope = ", round(fixed_effects["tap"], digits = 6),
      "\np-value = ", round(p, digits = 6)))
  ## scatterplot: trend
  # filter data
  data_integral_as_slope <- data_integral_as %>% 
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
  # fit lme
  lm_slope_integral_as <- lm(
    slope ~ slope_tap,
    data = data_integral_as_slope)
  summary(lm_slope_integral_as)
  predictions <- predict(lm_slope_integral_as, interval = "confidence", level = 0.95)
  slope_var <- data_integral_as_slope %>% 
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
    ylab("trend of log(as integral)")+
    ggtitle(paste0(
      "slope = ", round(summary(lm_slope_integral_as)$coefficient[["slope_tap", "Estimate"]], digits = 6),
      "\nr-squared = ", round(summary(lm_slope_integral_as)$r.squared, digits = 6),
      "\np-value = ", round(summary(lm_slope_integral_as)$coefficient[["slope_tap", "Pr(>|t|)"]], digits = 6)))
  df_plot$a[[i]] <- p_a
  df_plot$b[[i]] <- p_b
}
pdf(
  "output/figures/p_scatter_log(integral_as)_tap.pdf",
  width = 16,
  height = 6
)
for (i in 1:3) {
  grid.arrange(df_plot$a[[i]], df_plot$b[[i]],
               ncol = 2,
               widths = c(1,1))
}
dev.off()