calc_trend_ctnt <- function(df_in) {
  tryCatch({
    m_ctnt <- lme(
      Value ~ year_new,
      data = df_in,
      random = ~ year_new | n)
    return(m_ctnt)
  }, error = function(e) {
    cat("Error in lme formula:", conditionMessage(e), "\n")
    # Retry with the control parameter
    m_ctnt <- lme(
      Value ~ year_new,
      data = df_in,
      random = ~ year_new | n,
      control = lmeControl(opt = "optim"))
    return(m_ctnt)
  })
}