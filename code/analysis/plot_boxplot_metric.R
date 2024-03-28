# set order
df <- df_ana
df$Metric <- factor(df$Metric, levels = c("Ca", "Cp", "AIn", "ASIn", "LAS", "EAS", "SAS", "LOS", "EOS", "SOS"))

p_metrics_con <- ggplot(data = df %>% filter(Metric %in% c("Ca", "Cp")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration (spores m"^-3*")")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12))

p_metrics_pheno <- ggplot(data = df %>% filter(Metric %in% c("SOS", "EOS", "LOS", "SAS", "EAS", "LAS")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Day of spore year / Days") +
  scale_y_continuous(
    limits = c(-15, 406),
    breaks = c(1, 100, 200, 300, 365),
    position = "right") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = "black", size = 12)) +
  coord_flip()

p_metrics_in <- ggplot(data = df %>% filter(Metric %in% c("AIn", "ASIn")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration * days (spores m"^-3*" days)")) +
  scale_y_continuous(
    trans = "log10",
    breaks = c(10^4, 10^5, 10^6, 10^7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12))
