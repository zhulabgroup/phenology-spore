df_pheno_group <- df_pheno %>%
  filter(location %in% site_list) %>%
  drop_na(year) %>%
  distinct(location, year)

if (FALSE) {
  df_simplefit_list <- vector(mode = "list", length = nrow(df_pheno_group))
  for (i in 1:nrow(df_pheno_group)) {
    df_pheno_1y <- df_pheno %>%
      right_join(df_pheno_group %>% slice(i), by = c("location", "year")) %>%
      mutate(logcount = log(count + 1))
    set.seed(1)
    fit <- greenbrown::FitDoubleLogElmore(df_pheno_1y$logcount, plot = F, hessian = F, ninit = 50)
    df_pheno_1y <- df_pheno_1y %>%
      mutate(fit = fit$predicted %>% as.numeric())
    df_simplefit_list[[i]] <- df_pheno_1y
    print(str_c(i, " out of ", nrow(df_pheno_group)))
  }
  df_simplefit <- bind_rows(df_simplefit_list)
  write_rds(df_simplefit, str_c(.path$out_simple_model, "elmore.rds"))
}

df_simplefit <- read_rds(str_c(.path$out_simple_model, "elmore.rds"))

p_simple <- ggplot(df_simplefit) +
  geom_point(aes(x = doy + 60, y = exp(logcount) - 1, col = year, group = year), alpha = 0.1) +
  geom_line(aes(x = doy + 60, y = exp(fit) - 1, col = year, group = year), linewidth = 1) +
  theme_classic() +
  facet_wrap(. ~ location, scale = "free_y", ncol = 3) +
  guides(col = F) +
  scale_color_viridis_c() +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_continuous(labels = labelfunc_x) +
  # ggtitle("Phenology curves of Cladosporiaceae spores")+
  ylab(expression(Spore ~ concentration ~ (grains / m^3))) +
  xlab("Time of year")

# cairo_pdf("./nab/output/figures/3 simple_fit.pdf", width=6, height=6)
# print (p_simple)
# dev.off()
