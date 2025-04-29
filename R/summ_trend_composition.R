#' @export
calc_comm_comp <- function(df_spore, df_full) {
  # 55 stations included in our analysis
  df_station <- df_spore %>%
    left_join(
      df_full %>%
        distinct(n, .keep_all = T) %>%
        select(station, state, n),
      by = c("station", "state")
    ) %>%
    drop_na(n)

  # calculate relative abundance for each family per year
  df_comm <- df_station %>%
    filter(family != "Total" | is.na(family)) %>%
    mutate(year = format(date, "%Y") %>% as.integer()) %>%
    group_by(lat, lon, station, city, state, country, id, n, year) %>%
    summarise(total = sum(count, na.rm = T)) %>%
    ungroup() %>%
    right_join(
      df_station %>% mutate(year = format(date, "%Y") %>% as.integer()),
      by = c("lat", "lon", "station", "city", "state", "country", "id", "n", "year")
    ) %>%
    mutate(family = ifelse(is.na(family), "Unidentified", family)) %>%
    group_by(lat, lon, station, city, state, country, id, n, year, total, family) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    mutate(pctg = count / total) %>%
    arrange(desc(pctg)) %>%
    ungroup() %>%
    select(n, family, year, total, pctg)

  return(df_comm)
}

# fit lme
calc_trend_taxa <- function(df_in, fml) {
  m_taxa <- nlme::lme(
    pctg ~ year,
    data = df_in %>% filter(family == fml) %>% filter(total != 0),
    random = ~ year | n, control = nlme::lmeControl(opt = "optim")
  )

  sum_m <- summary(m_taxa)
  beta0SD <- diag(sqrt(nlme::getVarCov(m_taxa)))[["(Intercept)"]] %>%
    as.numeric() %>%
    signif(digits = 2)
  beta1SD <- diag(sqrt(nlme::getVarCov(m_taxa)))[["year"]] %>%
    as.numeric() %>%
    signif(digits = 2)
  rsdlSD <- sum_m$sigma %>%
    as.numeric() %>%
    signif(digits = 2)
  b0 <- nlme::fixef(m_taxa)[["(Intercept)"]] %>%
    as.numeric() %>%
    signif(digits = 2)
  b1 <- nlme::fixef(m_taxa)[["year"]] %>%
    as.numeric() %>%
    signif(digits = 2)
  CI <- nlme::intervals(m_taxa, which = "fixed")
  CI1 <- CI$fixed[2, "lower"] %>%
    as.numeric() %>%
    signif(digits = 2)
  CI2 <- CI$fixed[2, "upper"] %>%
    as.numeric() %>%
    signif(digits = 2)
  b0SE <- sum_m$tTable["(Intercept)", "Std.Error"] %>%
    as.numeric() %>%
    signif(digits = 2)
  b1SE <- sum_m$tTable["year", "Std.Error"] %>%
    as.numeric() %>%
    signif(digits = 2)
  b0t <- sum_m$tTable["(Intercept)", "t-value"] %>%
    as.numeric() %>%
    signif(digits = 2)
  b1t <- sum_m$tTable["year", "t-value"] %>%
    as.numeric() %>%
    signif(digits = 2)
  p <- sum_m$tTable[["year", "p-value"]] %>%
    as.numeric() %>%
    signif(digits = 2)
  n_obsv <- nobs(m_taxa)
  # Create a list
  result <- c(fml, beta0SD, beta1SD, rsdlSD, b0, b1, b0SE, b1SE, b0t, b1t, p)
  return(result)
}

#' @export
summ_composition_trend <- function(df_comm, ls_family = c("Unidentified", "Cladosporiaceae")) {
  # generate table
  df_trend_taxa <- data.frame()
  for (fam in ls_family) {
    m_rslt <- calc_trend_taxa(df_in = df_comm, fml = fam)
    df_trend_taxa <- rbind(df_trend_taxa, m_rslt)
  }
  colnames(df_trend_taxa) <- c("Taxa", "beta0SD", "beta1SD", "rsdlSD", "b0", "b1", "b0SE", "b1SE", "b0t", "b1t", "p")

  return(df_trend_taxa)
}
