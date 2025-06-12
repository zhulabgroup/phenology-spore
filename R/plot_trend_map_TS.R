#' @export
plot_trend_map_TS <- function(df_in, ls_metric) {
  ls_p_map <- list()
  for (m in ls_metric) {
    df <- df_in %>%
      filter(metric == m)

    p_map <- ggplot() +
      geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
      geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(
        breaks = c(25, 35, 45),
        labels = function(x) paste0(x, "\u00B0 N")
      ) +
      scale_x_continuous(
        breaks = c(-120, -100, -80),
        labels = function(x) ifelse(x < 0, paste0(abs(x), "\u00B0 W"), paste0(x, "\u00B0 E"))
      ) +
      labs(x = "Longitude", y = "Latitude") +
      geom_point(
        data = df,
        aes(x = lon, y = lat, size = Nyear, fill = slope),
        shape = 21,
        col = "grey40",
        alpha = 1
      ) +
      scale_size_continuous(
        range = c(5, 7),
        breaks = round(seq(min(df$Nyear), max(df$Nyear), length.out = 5)),
        name = "Number\nof years"
      ) +
      theme(
        legend.position = c(0.01, 0.01),
        legend.box = "horizontal",
        legend.box.just = "bottom",
        legend.justification = c(0, 0),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank()
      ) +
      guides(
        size = "none",
        fill = guide_colorbar(reverse = TRUE)
      )

    # add color palette
    if (m == "SOS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "red", mid = "white", high = "blue", midpoint = 0,
          breaks = c(-8, -4, 0, 4),
          labels = c("-8  Earlier", "-4", "0", "4  Later")
        ) +
        labs(fill = expression("Theil-Sen trend of\nSOS (days/year)"))
    }
    if (m == "SAS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "red", mid = "white", high = "blue", midpoint = 0,
          breaks = c(-8, -4, 0, 4),
          labels = c("-8  Earlier", "-4", "0", "4  Later")
        ) +
        labs(fill = expression("Theil-Sen trend of\nSAS (days/year)"))
    }
    if (m == "EOS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "red", mid = "white", high = "blue", midpoint = 0,
          breaks = c(-2, 0, 2, 5),
          labels = c("-2  Earlier", "0", "2", "5  Later")
        ) +
        labs(fill = expression("Theil-Sen trend of\nEOS (days/year)"))
    }
    if (m == "EAS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "red", mid = "white", high = "blue", midpoint = 0,
          breaks = c(-20, -10, 0, 5),
          labels = c("-20  Earlier", "-10", "0", "5  Later")
        ) +
        labs(fill = expression("Theil-Sen trend of\nEAS (days/year)"))
    }
    if (m == "LOS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-5, 0, 5),
          labels = c("-5  Shorter", "0", "5  Longer")
        ) +
        labs(fill = expression("Theil-Sen trend of\nLOS (days/year)"))
    }
    if (m == "LAS") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-10, 0, 10),
          labels = c("-10  Shorter", "0", "10  Longer")
        ) +
        labs(fill = expression("Theil-Sen trend of\nLAS (days/year)"))
    }
    if (m == "ln_Ca") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-0.4, -0.2, 0, 0.2),
          labels = c("-0.4  Lower", "-0.2", "0", "0.2  Higher")
        ) +
        labs(fill = expression("Theil-Sen trend\nof ln(Ca)"))
    }
    if (m == "ln_Cp") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-0.2, -0.1, 0),
          labels = c("-0.2  Lower", "-0.1", "0")
        ) +
        labs(fill = expression("Theil-Sen trend\nof ln(Cp)"))
    }
    if (m == "ln_AIn") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-0.2, -0.1, 0),
          labels = c("-0.2  Lower", "-0.1", "0")
        ) +
        labs(fill = expression("Theil-Sen trend\nof ln(AIn)"))
    }
    if (m == "ln_ASIn") {
      p_map <- p_map +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red", midpoint = 0,
          breaks = c(-0.1, -0.05, 0, 0.05),
          labels = c("-0.1  Lower", "-0.05", "0", "0.05  Higher")
        ) +
        labs(fill = expression("Theil-Sen trend\nof ln(ASIn)"))
    }

    ls_p_map[[m]] <- p_map
  }

  if (length(ls_p_map) == 1) {
    p_map_out <- ls_p_map[[1]]
  } else {
    p_map_out <- cowplot::plot_grid(
      plotlist = lapply(ls_p_map, function(p) {
        p + guides(color = "none")
      }),
      ncol = 2,
      labels = letters[1:length(ls_p_map)],
      label_fontface = "bold",
      label_size = 12
    )
  }
  return(p_map_out)
}

# Create a signed root transformation function
signed_root_trans <- function(power = 3) {
  scales::trans_new(
    name = paste0("signed_root_", power),
    transform = function(x) sign(x) * abs(x)^(1 / power),
    inverse = function(x) sign(x) * abs(x)^power,
    domain = c(-Inf, Inf)
  )
}
