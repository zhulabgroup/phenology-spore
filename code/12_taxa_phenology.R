taxa <- "Cladosporiaceae"
df_pheno <- df_comm_major %>%
  # filter(location %in% site_list) %>%
  filter(family == taxa) %>%
  select(-family) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  mutate(doy = as.integer(format(date, "%j"))) %>%
  group_by(location, year) %>%
  # mutate(cumsum=cumsum(coalesce(count_agg,0))) %>%
  ungroup() %>%
  filter(log(count + 1) > 0) %>%
  mutate(year = case_when(
    doy <= 60 ~ (year - 1),
    TRUE ~ as.double(year)
  )) %>%
  mutate(doy = case_when(
    doy > 60 ~ (doy - 60),
    TRUE ~ as.numeric(difftime(date, as.Date(paste0(year, "-01-01"))) - 60)
  )) %>%
  select(-date) %>%
  group_by(location, year) %>%
  tidyr::complete(doy = seq(1, 365, 1)) %>%
  right_join(
    (drop_na(.) %>%
      summarise(n = n()) %>%
      filter(n >= 365 / 3) %>%
      select(location, year)),
    by = c("location", "year")
  ) %>%
  ungroup()


p_pheno <- ggplot(df_pheno %>%
  filter(location %in% site_list)) +
  geom_line(aes(x = doy + 60, y = count, col = year, group = year), alpha = 0.5) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::trans_breaks("log", function(x) exp(x)),
    labels = scales::trans_format("log", scales::math_format(e^.x))
  ) +
  scale_x_continuous(labels = labelfunc_x) +
  ggtitle("Phenology curves of Cladosporiaceae spores") +
  ylab(expression(Spore ~ concentration ~ (grains / m^3))) +
  xlab("Time of year") +
  scale_color_viridis_c() +
  theme_classic() +
  facet_wrap(. ~ location, ncol = 3, scales = "free_y") #+
# guides(col="none")
p_pheno

# cairo_pdf("./nab/output/figures/2 pheno_curves.pdf", width=6, height=4)
# print (p_pheno)
# dev.off()
