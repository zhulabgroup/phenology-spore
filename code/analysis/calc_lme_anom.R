calc_lme_anom <- function(df_in) {
  tryCatch({
    m <- lme(
      value_anom ~ mat,
      data = df_in,
      random = ~ mat | n)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m <- lme(
      value_anom ~ mat,
      data = df_in,
      random = ~ mat | n,
      control = lmeControl(opt = "optim"))
  })
  
  return(m)
}