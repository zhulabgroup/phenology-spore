set.seed(42)
epoch=1

# neighbors and lags
var_list <- c("pollen"
              # ,"flower"
              # ,"leaf"
              # , "evi"
              # ,"climatology"
              , "tmean"
              , "prcp"
              # ,"vp"
)
vars <- 1:length(var_list)

var_oi<-1


maxgap<-rep(NA, length(var_list))
for (i in 1:length(var_list)) {
  if (var_list[i] == "evi") {
    maxgap[i] <- 16
  } else if (var_list[i] %in% c("leaf","flower", "pollen")) {
    maxgap[i] <- 28
  } else  {
    maxgap[i] <- 7
  }
}

whit_coef<-rep(NA, length(var_list))
for (i in 1:length(var_list)) {
  if (var_list[i] %in% c("leaf","flower", "evi")) {
    whit_coef[i] <- 0
  } else if (var_list[i] %in% c( "pollen")) {
    whit_coef[i] <- 100
  } else  {
    whit_coef[i] <- 10
  }
}

neighbors <- vector(mode = "list", length(vars))
for (i in 1:length(vars)) {
  if (var_list[i] == "doy") {
    neighbors[[i]] <- 1:1
  } # for doy
  else {
    neighbors[[i]] <- 1:1
  }
}

lags <- vector(mode = "list", length(vars))
for (i in 1:length(vars)) {
  if (var_list[vars[i]]  %in% c("doy", "climatology")) {
    lags[[i]] <- list(0)
  } else if (var_list[vars[i]]  %in% c("pollen")) {
    lags[[i]] <- list()
    for (period in 1:4) {
      lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
    }
  } else if (var_list[vars[i]]  %in% c("flower","leaf")) {
    lags[[i]] <-list(0)
  } else if (var_list[vars[i]]  =="evi") {
    lags[[i]] <- list()
    for (period in 1:13) {
      lags[[i]] <-rlist::list.append(lags[[i]] ,(seq(1,12*30+1, by=30)[period]-1)*1+0)
    }
  } else {
    lags[[i]] <- list()#list(1,2,3,4,5)
    for (period in 1:4) {
      lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
    }
  }
}

ndim <- 0
for (i in 1:length(vars)) {
  ndim <- ndim + length(neighbors[[i]]) * length(lags[[i]])
}


# Initial  hyperparameters
# https://chi-feng.github.io/gp-demo/
phimin <- 1e-50
phimax <- (2*pi)^2/2 # denominator is number of wiggles/zero crossings #1/0.2
vemin <- 0.001
vemax <-  0.25^2-vemin # 0.099
taumin <- 0.001
taumax <-  0.25^2-taumin#4.99
# rhomin <- .0001 
# rhomax <- 1-rhomin
gamma1min <- 1/100^2 # exp(-||s1-s2||^2*gamma1) gamma1 smaller -> u more similar over space
gamma1max <- 1/0.01^2
gamma2min <- 1/30^2 # exp(-||d1-d2||^2*gamma2) gamma2 smaller -> u more similar over time
gamma2max <- 1/1^2

V_list<-vector(mode="list")
for (v in 1:length(vars)){
  if (!is.null(lags[[v]])) {
    for (i in 1:length(lags[[v]])) {
      V_list <- rlist::list.append(V_list,0.1*exp(-(max(lags[[v]][[i]])/365)^2/5)) 
    }
  }
}
V_phi<-unlist(V_list)
priors <- list(
  E_phi = matrix(c(0 * rep(1, length = ndim))),
  # V_phi = matrix(0.05, ncol = 1, nrow = ndim),
  V_phi = V_phi, #informed prior
  # Other priors
  E_ve =  0,#0.25^2/2,
  V_ve = 5,
  E_tau = 0.25^2,#0.25^2/2,
  V_tau = 5,
  # E_rho = 1,
  # V_rho = 0.5,
  E_gamma1 = 0,
  V_gamma1 = 0.5,
  E_gamma2 = 0,
  V_gamma2 = 0.5
)

num_part <- 5

num_epoch <- 1 #20

basisnumber <-500

maxcount=200

num_sample=50
