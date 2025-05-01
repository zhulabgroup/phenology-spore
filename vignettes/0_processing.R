## ----setup, include=FALSE, message=FALSE--------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, cache = FALSE)

library(phenologyspore)

.fulldata <- F
.datsave <- F

if (exists(".fulldata") && isTRUE(.fulldata)) {
  .path <- list(
    input = "../alldata/input/",
    intermediate = "../alldata/intermediate/",
    output = "../alldata/output/"
  )
}

## ----read---------------------------------------------------------------------
if (.fulldata) {
  df_spore <- read_rds(str_c(.path$input, "dat_spore.rds"))
}

## ----process nab data---------------------------------------------------------
if (.fulldata) {
  df_full <- tidy_nab_data(df_spore, f = "Total", n_m = 10, n_yr = 3, rename = T, pad = T)
  if (.datsave) {
    write_rds(df_full, str_c(.path$intermediate, "full_raw.rds"))
  }
}

## ----meta---------------------------------------------------------------------
if (.fulldata) {
  df_meta <- df_full %>%
    distinct(n, .keep_all = T) %>%
    select(lat, lon, station, city, state, country, id, n)
  if (.datsave) {
    write_rds(df_meta, str_c(.path$intermediate, "meta.rds"))
    usethis::use_data(df_meta, overwrite = T)
  }
}

## ----process time series------------------------------------------------------
if (.fulldata) {
  df_ts <- tidy_nab_ts(df_full, path_offset = str_c(.path$intermediate, "fill_smooth_offset.rds"))
  if (.datsave) {
    write_rds(df_ts, str_c(.path$intermediate, "ts.rds"))
  }
}

## ----metrics------------------------------------------------------------------
if (.fulldata) {
  df_metrics <- calc_metrics(df_ts)
  if (.datsave) {
    write_rds(df_metrics, str_c(.path$intermediate, "metrics.rds"))
    usethis::use_data(df_metrics, overwrite = T)
  }
}

## ----download daymet----------------------------------------------------------
if (.fulldata) {
  df_daymet_raw <- down_daymet(df_meta, path = str_c(.path$input, "dat_daymet_raw.rds"))
}

## ----climate summary----------------------------------------------------------
if (.fulldata) {
  df_daymet_annual <- summ_climate(df_daymet_raw = df_daymet_raw, df_spore = df_ts)
  if (.datsave) {
    write_rds(df_daymet_annual, str_c(.path$intermediate, "daymet_annual.rds"))
    usethis::use_data(df_daymet_annual, overwrite = T)
  }
}

## ----tidy metric clim---------------------------------------------------------
df_analysis <- tidy_metric_clim(df_metrics, df_daymet_annual, pct = 0.8)
if (.datsave) {
  write_rds(df_analysis, str_c(.path$intermediate, "analysis.rds"))
  usethis::use_data(df_analysis, overwrite = T)
}

