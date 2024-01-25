calc_lme_anom <- function(df_in) {
  tryCatch({
    m <- lme(
      value_anom ~ climate_anom,
      data = df_in,
      random = ~ climate_anom | n)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m <- lme(
      value_anom ~ climate_anom,
      data = df_in,
      random = ~ climate_anom | n,
      control = lmeControl(opt = "optim"))
  })
  
  return(m)
}