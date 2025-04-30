#' @export
calc_sens_lamda <- function(df_full, path_offset = str_c(.path$intermediate, "fill_smooth_offset.rds"),
                            ls_lmd = c(10, seq(50, 500, by = 50))) {
  ls_df_lme <- list()
  for (lmd in ls_lmd) {
    df_ts <- tidy_nab_ts(df_full, path_offset = path_offset, n_gap = 14, lambda = lmd)

    df_metrics <- calc_metrics(df_ts)

    ls_df_lme[[lmd %>% as.character()]] <- calc_lme_all(df_metrics, x_vrb = "year_new", pct = 0.8) %>%
      mutate(lambda = lmd)
  }
  df_lme <- bind_rows(ls_df_lme)

  return(df_lme)
}

#' @export
plot_sens_lambda <- function(df_lme_lambda) {
  df_sens_l_slope <- df_lme_lambda %>%
    group_by(metric) %>%
    do({
      result <- lm(beta ~ lambda, .)
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
    rename("slope" = "lambda", "intercept" = "(Intercept)") %>%
    ungroup() %>%
    right_join(df_lme_lambda, by = "metric") %>%
    mutate(facet_label = paste0("slope = ", round(slope, 5), "  *p* = ", round(p_value, 5))) %>%
    mutate(metric = factor(metric, levels = c("SOS", "SAS", "EOS", "EAS", "LOS", "LAS", "ln_Ca", "ln_Cp", "ln_AIn", "ln_ASIn")))

  p_sens_l <- ggplot(data = df_sens_l_slope, aes(x = lambda, y = beta)) +
    geom_hline(aes(yintercept = 0), col = "gray") +
    geom_point(col = "dark red") +
    geom_errorbar(aes(ymin = ci1, ymax = ci2), col = "dark red", width = 0) +
    scale_x_continuous(breaks = c(10, seq(50, 500, by = 50))) +
    xlab(expression(italic(lambda))) +
    ylab(expression(italic(beta))) +
    facet_wrap(~ metric + facet_label, ncol = 2, scales = "free") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = ggtext::element_markdown(hjust = 0),
      text = element_text(size = 12),
      axis.title.y = element_text(angle = 0, vjust = 0.5)
    )

  return(p_sens_l)
}
