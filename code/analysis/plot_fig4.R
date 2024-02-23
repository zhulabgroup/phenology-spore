source("~/Github/spore_phenology/code/analysis/calc_lme_climate.R")
source("~/Github/spore_phenology/code/analysis/plot_lme_climate_pred.R")

# fit lme model using climate data
y = "ln_AIn"
x = "MAT"

m_lme_climate <- calc_lme_climate(
  df_in = df_ana,
  metric = y,
  x_lab = x,
  pct = 0.8)
summary(m_lme_climate)

p_lme_climate <- plot_lme_climate_pred(
  m_lme = m_lme_climate,
  df_in = df_ana,
  metric = y,
  x_lab = x,
  pct = 0.8)
p_lme_climate
