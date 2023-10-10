# Return an array whose columns contain the wavelet
# basis vectors on R^n, where n is a power of 2.
get_basis = function(qmf, n) {
  W = array(0, c(n, n))
  for (j in 1:n) {
    Y = array(0, n)
    Y[j] = 1
    W[,j] = IWT_PO(Y, 0, qmf)
  }
  return(W)
}

# Denoise the observed signal y, which must align
# with a gridded domain, but may contain missing
# values (NA).  The factor wf modifies the smoothing
# parameter, e.g. wf=0.5 uses a smoothing parameter
# that is half as large as that selected by cross
# validation.
denoise = function(y, qmf, wf=1) {
  
  # Generate a basis on the appropriate domain.
  n = length(y)
  m = as.integer(ceiling(log(n) / log(2)))
  m = 2^m
  W = get_basis(qmf, m)
  
  # Pad y to align with W.
  y = c(y, rep(NA, m - n))
  
  # Fit to the observed data, use CV to
  # select a preliminary penalty weight.
  ii = is.finite(y)
  r = cv.glmnet(W[ii,], y[ii])
  
  # Refit with the final penalty weight.
  la = r$lambda.min * wf
  r2 = glmnet(W[ii,], y[ii], lambda=la)
  return(predict(r2, W)[1:n])
}

qmf = MakeONFilter('Symmlet', 6)

tidy_smoothwavelet_station <- function(df_raw, column_name, new_column_name) {
  df <- df_raw %>% 
  group_by(lat, lon, station, city, state, country, id, n) %>%
  mutate({{new_column_name}} := denoise({{column_name}}, qmf)) %>% 
  ungroup()
  
  return(df)
}