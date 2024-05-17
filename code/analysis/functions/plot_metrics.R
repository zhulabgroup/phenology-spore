# df_metrics <- read_rds(str_c(.path$dat_process, "2023-04-25/metrics_offset_flags.rds"))

# # 100 lambda generates unsmoothed curve
# source("~/Github/spore_phenology/code/analysis/plot_antt_station_spyr.R")
# p_antt <- plot_antt_station_spyr(df_in = df_sporeyr, sample_city = "Sarasota", spyr = 2008)

source("~/Github/spore_phenology/code/analysis/plot_antt.R")
source("~/Github/spore_phenology/code/analysis/plot_boxplot_metric.R")
p_metrics <- plot_grid(p_antt, p_metrics_con, p_metrics_pheno, p_metrics_in,
          ncol = 2, labels = c("(a)", "(b)", "(c)", "(d)"), label_fontface = "bold", label_size = 12,
          rel_heights = c(1, 1), rel_widths = c(2, 1))

# p_metrics <- p_antt + p_metrics_con + p_metrics_pheno + p_metrics_in +
#   plot_layout(ncol = 2, widths = c(2, 1)) +
#   plot_annotation(
#     tag_levels = 'A',
#     theme = theme(plot.title = element_text(size = 12)))