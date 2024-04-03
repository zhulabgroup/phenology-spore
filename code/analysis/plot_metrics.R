# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

# # 100 lambda generates unsmoothed curve
# source("~/Github/spore_phenology/code/analysis/plot_antt_station_spyr.R")
# p_antt <- plot_antt_station_spyr(df_in = df_sporeyr, sample_city = "Sarasota", spyr = 2008)

source("~/Github/spore_phenology/code/analysis/plot_antt.R")
source("~/Github/spore_phenology/code/analysis/plot_boxplot_metric.R")
p_metrics <- plot_grid(p_antt, p_metrics_con, p_metrics_pheno, p_metrics_in,
          ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
          rel_heights = c(1, 1), rel_widths = c(2, 1))

# p_metrics <- p_antt + p_metrics_con + p_metrics_pheno + p_metrics_in +
#   plot_layout(ncol = 2, widths = c(2, 1)) +
#   plot_annotation(
#     tag_levels = 'A',
#     theme = theme(plot.title = element_text(size = 12)))





source("~/Github/spore_phenology/code/analysis/plot_antt0.R")
p_metrics0 <- plot_grid(p_antt0, p_metrics_con, p_metrics_pheno, p_metrics_in,
                       ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                       rel_heights = c(1, 1), rel_widths = c(2, 1))

source("~/Github/spore_phenology/code/analysis/plot_antt1.R")
p_metrics1 <- plot_grid(p_antt1, p_metrics_con, p_metrics_pheno, p_metrics_in,
                        ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                        rel_heights = c(1, 1), rel_widths = c(2, 1))

source("~/Github/spore_phenology/code/analysis/plot_antt2.R")
p_metrics2 <- plot_grid(p_antt2, p_metrics_con, p_metrics_pheno, p_metrics_in,
                        ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                        rel_heights = c(1, 1), rel_widths = c(2, 1))

source("~/Github/spore_phenology/code/analysis/plot_antt3.R")
p_metrics3 <- plot_grid(p_antt3, p_metrics_con, p_metrics_pheno, p_metrics_in,
                        ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                        rel_heights = c(1, 1), rel_widths = c(2, 1))

source("~/Github/spore_phenology/code/analysis/plot_antt4.R")
p_metrics4 <- plot_grid(p_antt4, p_metrics_con, p_metrics_pheno, p_metrics_in,
                        ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                        rel_heights = c(1, 1), rel_widths = c(2, 1))

source("~/Github/spore_phenology/code/analysis/plot_antt5.R")
p_metrics5 <- plot_grid(p_antt5, p_metrics_con, p_metrics_pheno, p_metrics_in,
                        ncol = 2, labels = c("A", "B", "C", "D"), label_fontface = "plain", label_size = 12,
                        rel_heights = c(1, 1), rel_widths = c(2, 1))
