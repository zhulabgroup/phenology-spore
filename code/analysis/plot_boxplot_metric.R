# set order
df_ana$Metric <- factor(df_ana$Metric, levels = c("Cp", "A", "AIn", "ASIn", "LAS", "EAS", "SAS", "LOS", "EOS", "SOS"))

p_metrics_con <- ggplot(data = df_ana %>% filter(Metric %in% c("Cp", "A")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration (grains m"^-3*")")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")) +
  labs(title = expression(paste(bold("B")))) +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10))

p_metrics_pheno <- ggplot(data = df_ana %>% filter(Metric %in% c("SOS", "EOS", "LOS", "SAS", "EAS", "LAS")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Day of spore year") +
  scale_y_continuous(
    limits = c(-15, 406),
    breaks = c(1, 100, 200, 300, 365),
    position = "right") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")) +
  labs(title = expression(paste(bold("C")))) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b = -10)),
    plot.margin = margin(0, 10, 10, 10))+
  coord_flip()

p_metrics_in <- ggplot(data = df_ana %>% filter(Metric %in% c("AIn", "ASIn")), aes(x = Metric, y = Value)) +
  geom_boxplot(width = 0.3) +
  theme_classic() +
  ylab(expression("Spore concentration * days (grains m"^-3*" days)")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y = element_text(color = "black")) +
  labs(title = expression(paste(bold("D")))) +
  theme(plot.title.position = "plot",
        plot.margin = margin(10, 0, 10, 10))