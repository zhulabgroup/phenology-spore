df_trend_flag <- read_csv(str_c(.path$dat_process, "2023-04-25/trend_flag_tab.csv"), col_names = F)
X4.1 <- c("100%", "Number of observations", 160, 162, 91, 162, 162, 215, 91)
X7.1 <- c("90%", "Number of observations", 211, 222, 100, NA, NA, NA, 100)
X10.1 <- c("80%", "Number of observations", 241, 266, 103, NA, NA, NA, 103)
X13.1 <- c("70%", "Number of observations", 260, 292, NA, NA, NA, NA, NA)
df_trend_flag$X4.1 <- X4.1
df_trend_flag$X7.1 <- X7.1
df_trend_flag$X10.1 <- X10.1
df_trend_flag$X13.1 <- X13.1
df_trend_flag <- df_trend_flag %>% 
  dplyr::select(X1, X2, X3, X4, X4.1, X5, X6, X7, X7.1, X8, X9, X10, X10.1, X11, X12, X13, X13.1)
