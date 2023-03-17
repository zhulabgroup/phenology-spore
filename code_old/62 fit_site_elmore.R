library(nimble, warn.conflicts = FALSE)
p_load(daymetr)
p_load(lubridate)
########################
# Design model
########################
df_pheno_model<-df_pheno_subset %>% 
  drop_na() %>% 
  filter(id==35|id==23|id==49)%>%
  mutate(id=factor(id)) %>%  
  mutate(level=as.integer(id))

n<-df_pheno_model %>% pull(level) %>% unique() %>% length()

df_daymet<-vector(mode="list", length=n)
for ( i in 1:n) {
  meta<-df_pheno_model %>% 
    filter(level==i) %>% 
    slice(1)
  df_daymet[[i]] <- download_daymet(site = meta$station,
                                    lat = meta$lat,
                                    lon = meta$lon,
                                    start = year(min(df_pheno_model$Date)),
                                    end = min(year(max(df_pheno_model$Date)), 2020),
                                    internal = TRUE,
                                    simplify = TRUE) %>% 
    
    mutate(level=i) %>%
    filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.","prcp..mm.day.")) %>% 
    spread(key = "measurement", value="value") %>% 
    rename(prcp=`prcp..mm.day.`,
           tmax=`tmax..deg.c.`,
           tmin=`tmin..deg.c.`) %>% 
    mutate(Date=as.Date(yday, origin = paste0(year,"-01-01"))-1) %>% 
    mutate (temp=(tmax+tmin/2)) %>% 
    dplyr::select(level, Date, temp, prcp)
}
df_daymet<-bind_rows(df_daymet)

df_daymet_annual<-df_daymet %>% 
  left_join(df_pheno_model %>% distinct(level,Date, year), by=c("level","Date") )%>% 
  drop_na(year) %>% 
  group_by(level,year) %>% 
  summarize(mat=mean(temp),
            tap=sum(prcp)) %>% 
  ungroup()

df_pheno_model<-df_pheno_model %>% 
  left_join(df_daymet_annual, by=c("level","year" )) %>% 
  drop_na(tap, mat)

df_pheno_model<-df_pheno_model%>% 
  mutate(tap=(tap-mean(df_pheno_model$tap))/(max(df_pheno_model$tap)-min(df_pheno_model$tap))) %>% 
  mutate(mat=(mat-mean(df_pheno_model$mat))/(max(df_pheno_model$mat)-min(df_pheno_model$mat))) %>% 
  mutate(lat=(lat-mean(df_pheno_model$lat))/(max(df_pheno_model$lat)-min(df_pheno_model$lat))) %>% 
  mutate(year=(year-mean(df_pheno_model$year))/(max(df_pheno_model$year)-min(df_pheno_model$year)))

N<-nrow(df_pheno_model)

#####
mn_min = -5
mn_max=5
mx_min = 5
mx_max=15
sos_min=0
sos_max=180
eos_min=180
eos_max=365
rsp_min=5
rsp_max=80
rau_min=5
rau_max=80
rsu_min=-0.01
rsu_max=0.01

Code <- nimbleCode({
  sigma~dgamma(1,0.1)
  
  mu_mn ~ dnorm(mean=0, var=1)
  mu_mx ~ dnorm(mean=0, var=1)
  mu_sos ~ dnorm(mean=0, var=1)
  mu_eos ~ dnorm(mean=0, var=1)
  mu_rsp ~ dnorm(mean=0, var=1)
  mu_rau ~ dnorm(mean=0, var=1)
  mu_rsu ~ dnorm(mean=0, var=1)
  
  beta1_mn ~ dnorm(mean=0, var=1)
  beta1_mx ~ dnorm(mean=0, var=1)
  beta1_sos ~ dnorm(mean=0, var=1)
  beta1_eos ~ dnorm(mean=0, var=1)
  beta1_rsp ~ dnorm(mean=0, var=1)
  beta1_rau ~ dnorm(mean=0, var=1)
  beta1_rsu ~ dnorm(mean=0, var=1)
  
  beta2_mn ~ dnorm(mean=0, var=1)
  beta2_mx ~ dnorm(mean=0, var=1)
  beta2_sos ~ dnorm(mean=0, var=1)
  beta2_eos ~ dnorm(mean=0, var=1)
  beta2_rsp ~ dnorm(mean=0, var=1)
  beta2_rau ~ dnorm(mean=0, var=1)
  beta2_rsu ~ dnorm(mean=0, var=1)
  
  sig_sq_mn ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_mx ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_sos ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_eos ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_rsp ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_rau ~ dgamma(shape = 1^2/1, scale= 10)
  sig_sq_rsu ~ dgamma(shape = 1^2/1, scale= 10)
  
  for (s in 1:n) {
    ep_mn_s [s] ~ dnorm(mean = 0, var = sig_sq_mn)
    ep_mx_s [s] ~ dnorm(mean = 0, var = sig_sq_mx)
    ep_sos_s [s] ~ dnorm(mean = 0, var = sig_sq_sos)
    ep_eos_s [s] ~ dnorm(mean = 0, var = sig_sq_eos)
    ep_rsp_s [s] ~ dnorm(mean = 0, var = sig_sq_rsp)
    ep_rau_s [s] ~ dnorm(mean = 0, var = sig_sq_rau)
    ep_rsu_s [s] ~ dnorm(mean = 0, var = sig_sq_rsu)
  }
  
  for (i in 1:N) {
    logit (t_mn[i]) <-mu_mn+ep_mn_s[site[i]]+beta1_mn*mat[i]+beta2_mn*tap[i]
    logit (t_mx[i]) <-mu_mx+ep_mx_s[site[i]]+beta1_mx*mat[i]+beta2_mx*tap[i]
    logit (t_sos[i]) <-mu_sos+ep_sos_s[site[i]]+beta1_sos*mat[i]+beta2_sos*tap[i]
    logit (t_eos[i]) <-mu_eos+ep_eos_s[site[i]]+beta1_eos*mat[i]+beta2_eos*tap[i]
    logit (t_rsp[i]) <-mu_rsp+ep_rsp_s[site[i]]+beta1_rsp*mat[i]+beta2_rsp*tap[i]
    logit (t_rau[i]) <-mu_rau+ep_rau_s[site[i]]+beta1_rau*mat[i]+beta2_rau*tap[i]
    logit (t_rsu[i]) <-mu_rsu+ep_rsu_s[site[i]]+beta1_rsu*mat[i]+beta2_rsu*tap[i]
    
    mn[i] <- t_mn[i] *(mn_max-mn_min) + mn_min
    mx[i] <- t_mx[i] *(mx_max-mx_min) + mx_min
    sos[i] <- t_sos[i] *(sos_max-sos_min) + sos_min
    eos[i] <- t_eos[i] *(eos_max-eos_min) + eos_min
    rsp[i] <- t_rsp[i] *(rsp_max-rsp_min) + rsp_min
    rau[i] <- t_rau[i] *(rau_max-rau_min) + rau_min
    rsu[i] <- t_rsu[i] *(rsu_max-rsu_min) + rsu_min
    
    y[i] ~dnorm(mn[i] + ((mx[i]-mn[i]) + rsu[i] * doy[i]) *(1/(1+exp((sos[i]-doy[i])/rsp[i]))-1/(1+exp((eos[i]-doy[i])/rau[i]))), sigma)
  }
})

Data <- list(
  y = log(df_pheno_model$count_agg+1),
  yr = df_pheno_model$year,
  doy = df_pheno_model$doy,
  site = df_pheno_model$level,
  lat = df_pheno_model$lat,
  mat = df_pheno_model$mat,
  tap = df_pheno_model$tap,
  constraint_data1=1,
  constraint_data2=1
)

Dimensions <- list(
  ep_mn_s=n,
  ep_mx_s=n,
  ep_sos_s=n,
  ep_eos_s=n,
  ep_rsp_s=n,
  ep_rau_s=n,
  ep_rsu_s=n
)

Inits <- list(
  sigma=0.1,
  
  mu_mn =0,
  mu_mx =0,
  mu_sos =0,
  mu_eos =0,
  mu_rsp =0,
  mu_rau=0,
  mu_rsu=0,
  
  beta1_mn =0,
  beta1_mx  =0,
  beta1_sos  =0,
  beta1_eos  =0,
  beta1_rsp  =0,
  beta1_rau  =0,
  beta1_rsu  =0,
  
  beta2_mn =0,
  beta2_mx  =0,
  beta2_sos  =0,
  beta2_eos  =0,
  beta2_rsp  =0,
  beta2_rau  =0,
  beta2_rsu  =0,
  
  sig_sq_mn =0.1,
  sig_sq_mx =0.1,
  sig_sq_sos  =0.1,
  sig_sq_eos =0.1,
  sig_sq_rsp  =0.1,
  sig_sq_rau  =0.1,
  sig_sq_rsu  =0.1,
  
  ep_mn_s=rep(0, n),
  ep_mx_s=rep(0,n),
  ep_sos_s=rep(0,n),
  ep_eos_s=rep(0,n),
  ep_rsp_s=rep(0,n),
  ep_rau_s=rep(0,n),
  ep_rsu_s=rep(0,n)
)


Constants <-list(n=n,
                 N=N,
                 mn_min = mn_min,
                 mn_max=mn_max,
                 mx_min = mx_min,
                 mx_max=mx_max,
                 sos_min=sos_min,
                 sos_max=sos_max,
                 eos_min=eos_min,
                 eos_max=eos_max,
                 rsp_min=rsp_min,
                 rsp_max=rsp_max,
                 rau_min=rau_min,
                 rau_max=rau_max,
                 rsu_min=rsu_min,
                 rsu_max=rsu_max
)# [N:(1+1+n)])
########################
# Fit model
########################

Model <- nimbleModel(code = Code,
                     data = Data,
                     inits = Inits,
                     dimensions = Dimensions,
                     constants = Constants,
                     check = F, calculate=F)
MCMCconfiguration <- configureMCMC(Model)
MCMCconfiguration$addMonitors("ep_mn_s[]",
                              "ep_mx_s[]",
                              "ep_sos_s[]",
                              "ep_eos_s[]",
                              "ep_rsp_s[]",
                              "ep_rau_s[]",
                              "ep_rsu_s[]"
)
mMCMC <- buildMCMC(MCMCconfiguration)
CModel <- compileNimble(Model, resetFunctions = T)
Cmcmc <- compileNimble(mMCMC, project=CModel, resetFunctions = T)

Cmcmc$run( niter = 1)
Cmcmc$mvSamples %>% as.matrix()

runMCMC_samples <- runMCMC(Cmcmc, nburnin = 200, niter = 1000)
write_csv(as.data.frame(runMCMC_samples), "./nab/mcmc.csv")

########################
# Check MCMC samples
########################

runMCMC_samples_view <- as.matrix(read_csv("./nab/mcmc.csv"))# [4001:8000,]
runMCMC_samples_view %>% tail()
i <- 'beta1_sos'
# i=11
plot(runMCMC_samples_view[, i], type = "l", xlab = "iteration", ylab = colnames(runMCMC_samples_view)[i])
plot(density(runMCMC_samples_view[, i]))
median(runMCMC_samples_view[, i])
mean(runMCMC_samples_view[, i])
var(runMCMC_samples_view[, i])

params<-apply(runMCMC_samples_view,2,median)
params


dl<-function (doy
              ,mat, tap
              ,site, params,n) {
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  # beta_mn<-params['beta_mn']
  # beta_mx<-params['beta_mx']
  # beta_sos<-params['beta_sos']
  # beta_eos<-params['beta_eos']
  # beta_rsp<-params['beta_rsp']
  # beta_rau<-params['beta_rau']
  
  beta1_mn<-params['beta1_mn']
  beta1_mx<-params['beta1_mx']
  beta1_sos<-params['beta1_sos']
  beta1_eos<-params['beta1_eos']
  beta1_rsp<-params['beta1_rsp']
  beta1_rau<-params['beta1_rau']
  beta1_rsu<-params['beta1_rsu']
  
  beta2_mn<-params['beta2_mn']
  beta2_mx<-params['beta2_mx']
  beta2_sos<-params['beta2_sos']
  beta2_eos<-params['beta2_eos']
  beta2_rsp<-params['beta2_rsp']
  beta2_rau<-params['beta2_rau']
  beta2_rsu<-params['beta2_rsu']
  
  ep_mn_s<-params[paste0('ep_mn_s[',1:n,']')] 
  ep_mx_s<-params[paste0('ep_mx_s[',1:n,']')]
  ep_sos_s<-params[paste0('ep_sos_s[',1:n,']')]
  ep_eos_s<-params[paste0('ep_eos_s[',1:n,']')]
  ep_rsp_s<-params[paste0('ep_rsp_s[',1:n,']')]
  ep_rau_s<-params[paste0('ep_rau_s[',1:n,']')]
  ep_rsu_s<-params[paste0('ep_rsu_s[',1:n,']')]
  
  ep_mn_s[is.na(ep_mn_s)]<-0
  ep_mx_s[is.na(ep_mx_s)]<-0
  ep_sos_s[is.na(ep_sos_s)]<-0
  ep_eos_s[is.na(ep_eos_s)]<-0
  ep_rsp_s[is.na(ep_rsp_s)]<-0
  ep_rau_s[is.na(ep_rau_s)]<-0
  ep_rsu_s[is.na(ep_rsu_s)]<-0
  
  logit_t_mn <-mu_mn+ep_mn_s[site]+beta1_mn*mat+beta2_mn*tap
  logit_t_mx <-mu_mx+ep_mx_s[site]+beta1_mx*mat+beta2_mx*tap
  logit_t_sos <-mu_sos+ep_sos_s[site]+beta1_sos*mat+beta2_sos*tap
  logit_t_eos <-mu_eos+ep_eos_s[site]+beta1_eos*mat+beta2_eos*tap
  logit_t_rsp <-mu_rsp+ep_rsp_s[site]+beta1_rsp*mat+beta2_rsp*tap
  logit_t_rau <-mu_rau+ep_rau_s[site]+beta1_rau*mat+beta2_rau*tap
  logit_t_rsu <-mu_rsu+ep_rsu_s[site]+beta1_rsu*mat+beta2_rsu*tap
  
  t_mn<-exp(logit_t_mn)/(1+exp(logit_t_mn))
  t_mx<-exp(logit_t_mx)/(1+exp(logit_t_mx))
  t_sos<-exp(logit_t_sos)/(1+exp(logit_t_sos))
  t_eos<-exp(logit_t_eos)/(1+exp(logit_t_eos))
  t_rsp<-exp(logit_t_rsp)/(1+exp(logit_t_rsp))
  t_rau<-exp(logit_t_rau)/(1+exp(logit_t_rau))
  t_rsu<-exp(logit_t_rsu)/(1+exp(logit_t_rsu))
  
  mn <- t_mn *(mn_max-mn_min) + mn_min
  mx <- t_mx *(mx_max-mx_min) + mx_min
  sos <- t_sos *(sos_max-sos_min) + sos_min
  eos <- t_eos *(eos_max-eos_min) + eos_min
  rsp <- t_rsp *(rsp_max-rsp_min) + rsp_min
  rau <- t_rau *(rau_max-rau_min) + rau_min
  rsu <- t_rsu *(rsu_max-rsu_min) + rsu_min
  
  y=mn + ((mx-mn) + rsu * doy) *(1/(1+exp((sos-doy)/rsp))-1/(1+exp((eos-doy)/rau)))
  
  return(y)
}

params_init<-c(
  mu_mn=Inits$mu_mn,
  mu_mx=Inits$mu_mx,
  mu_sos=Inits$mu_sos,
  mu_eos=Inits$mu_eos,
  mu_rsp=Inits$mu_rsp,
  mu_rau=Inits$mu_rau,
  mu_rsu=Inits$mu_rsu,
  
  # beta_mn=Inits$beta_mn,
  # beta_mx=Inits$beta_mx,
  # beta_sos=Inits$beta_sos,
  # beta_eos=Inits$beta_eos,
  # beta_rsp=Inits$beta_rsp,
  # beta_rau=Inits$beta_rau
  
  beta1_mn=Inits$beta1_mn,
  beta1_mx=Inits$beta1_mx,
  beta1_sos=Inits$beta1_sos,
  beta1_eos=Inits$beta1_eos,
  beta1_rsp=Inits$beta1_rsp,
  beta1_rau=Inits$beta1_rau,
  beta1_rsu=Inits$beta1_rsu,
  
  beta2_mn=Inits$beta2_mn,
  beta2_mx=Inits$beta2_mx,
  beta2_sos=Inits$beta2_sos,
  beta2_eos=Inits$beta2_eos,
  beta2_rsp=Inits$beta2_rsp,
  beta2_rau=Inits$beta2_rau,
  beta2_rsu=Inits$beta2_rsu
)

df<-df_pheno_model %>% 
  # mutate(y_init=dl(doy, year-mean(df_pheno_model$year), level, params_init, n)) %>% 
  mutate(y_init=dl(doy, mat,tap, level, params_init, n)) %>%
  # mutate(y_pred=dl(doy,year-mean(df_pheno_model$year),level,params, n))# %>% 
  mutate(y_pred=dl(doy,mat,tap,level,params, n))


get_params<-function(params,n){
  
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  beta1_mn<-params['beta1_mn']
  beta1_mx<-params['beta1_mx']
  beta1_sos<-params['beta1_sos']
  beta1_eos<-params['beta1_eos']
  beta1_rsp<-params['beta1_rsp']
  beta1_rau<-params['beta1_rau']
  beta1_rsu<-params['beta1_rsu']
  
  beta2_mn<-params['beta2_mn']
  beta2_mx<-params['beta2_mx']
  beta2_sos<-params['beta2_sos']
  beta2_eos<-params['beta2_eos']
  beta2_rsp<-params['beta2_rsp']
  beta2_rau<-params['beta2_rau']
  beta2_rsu<-params['beta2_rsu']
  
  ep_mn_s<-params[paste0('ep_mn_s[',1:n,']')] 
  ep_mx_s<-params[paste0('ep_mx_s[',1:n,']')]
  ep_sos_s<-params[paste0('ep_sos_s[',1:n,']')]
  ep_eos_s<-params[paste0('ep_eos_s[',1:n,']')]
  ep_rsp_s<-params[paste0('ep_rsp_s[',1:n,']')]
  ep_rau_s<-params[paste0('ep_rau_s[',1:n,']')]
  ep_rsu_s<-params[paste0('ep_rsu_s[',1:n,']')]
  
  ep_mn_s[is.na(ep_mn_s)]<-0
  ep_mx_s[is.na(ep_mx_s)]<-0
  ep_sos_s[is.na(ep_sos_s)]<-0
  ep_eos_s[is.na(ep_eos_s)]<-0
  ep_rsp_s[is.na(ep_rsp_s)]<-0
  ep_rau_s[is.na(ep_rau_s)]<-0
  ep_rsu_s[is.na(ep_rsu_s)]<-0
  
  params_all<-vector(mode="list", length=n)
  for (site in 1:n) {
    logit_t_mn <-mu_mn+ep_mn_s[site]#+beta1_mn*mat+beta2_mn*tap
    logit_t_mx <-mu_mx+ep_mx_s[site]#+beta1_mx*mat+beta2_mx*tap
    logit_t_sos <-mu_sos+ep_sos_s[site]#+beta1_sos*mat+beta2_sos*tap
    logit_t_eos <-mu_eos+ep_eos_s[site]#+beta1_eos*mat+beta2_eos*tap
    logit_t_rsp <-mu_rsp+ep_rsp_s[site]#+beta1_rsp*mat+beta2_rsp*tap
    logit_t_rau <-mu_rau+ep_rau_s[site]#+beta1_rau*mat+beta2_rau*tap
    logit_t_rsu <-mu_rsu+ep_rsu_s[site]#+beta1_rsu*mat+beta2_rsu*tap
    
    t_mn<-exp(logit_t_mn)/(1+exp(logit_t_mn))
    t_mx<-exp(logit_t_mx)/(1+exp(logit_t_mx))
    t_sos<-exp(logit_t_sos)/(1+exp(logit_t_sos))
    t_eos<-exp(logit_t_eos)/(1+exp(logit_t_eos))
    t_rsp<-exp(logit_t_rsp)/(1+exp(logit_t_rsp))
    t_rau<-exp(logit_t_rau)/(1+exp(logit_t_rau))
    t_rsu<-exp(logit_t_rsu)/(1+exp(logit_t_rsu))
    
    mn <- t_mn *(mn_max-mn_min) + mn_min
    mx <- t_mx *(mx_max-mx_min) + mx_min
    sos <- t_sos *(sos_max-sos_min) + sos_min
    eos <- t_eos *(eos_max-eos_min) + eos_min
    rsp <- t_rsp *(rsp_max-rsp_min) + rsp_min
    rau <- t_rau *(rau_max-rau_min) + rau_min
    rsu <- t_rsu *(rsu_max-rsu_min) + rsu_min
    
    params_all[[site]]<-data.frame(level=site, 
                                   mn=mn,
                                   mx=mx,
                                   sos=sos,
                                   eos=eos,
                                   rsp=rsp,
                                   rau=rau,
                                   rsu=rsu)
  }
  params_all<-bind_rows(params_all)
  
  return(params_all)
}


ggplot()+
  geom_line(data=df_pheno_model,aes(x=doy, y=log(count_agg+1), col=year,group=year),lty=2)+
  geom_line(data=df,aes(x=doy, y=y_pred, col=year,group=year))+
  # geom_line(data=df,aes(x=doy, y=y_init), col="blue")+
  # geom_line(data=df,aes(x=doy, y=y_mean), col="red")+
  # geom_line(data=df_pheno_mean,aes(x=doy, y=log(count_agg+1)), col="red")+
  theme_classic()+
  scale_color_viridis_c()+
  facet_wrap(.~level*station)

get_params(params, n)
get_params(params_init, n)

ggplot(df %>% mutate(level=as.factor(level)))+
  geom_point(aes(x=log(count_agg+1), y = y_pred, group=level, col=level), alpha=0.1)+
  geom_abline(intercept = 0, slope=1, col="red", lty=2)+
  coord_equal()+
  theme_classic()+
  facet_wrap(.~level*station)
cor(log(df$count_agg+1), df$y_pred)

get_betas<-function(MCMC) {
  beta1_mn<-data.frame(value=as.numeric(MCMC[,'beta1_mn'])) %>% 
    mutate(predictor="mat",
           param="mn") %>% 
    mutate(id=row_number())
  beta1_mx<-data.frame(value=as.numeric(MCMC[,'beta1_mx']))%>% 
    mutate(predictor="mat",
           param="mx") %>% 
    mutate(id=row_number())
  beta1_sos<-data.frame(value=as.numeric(MCMC[,'beta1_sos']))%>%
    mutate(predictor="mat",
           param="sos") %>% 
    mutate(id=row_number())
  beta1_eos<-data.frame(value=as.numeric(MCMC[,'beta1_eos'])) %>%
    mutate(predictor="mat",
           param="eos") %>% 
    mutate(id=row_number())
  beta1_rsp<-data.frame(value=as.numeric(MCMC[,'beta1_rsp'])) %>%
    mutate(predictor="mat",
           param="rsp") %>% 
    mutate(id=row_number())
  beta1_rau<-data.frame(value=as.numeric(MCMC[,'beta1_rau'])) %>%
    mutate(predictor="mat",
           param="rau") %>% 
    mutate(id=row_number())
  beta1_rsu<-data.frame(value=as.numeric(MCMC[,'beta1_rsu'])) %>%
    mutate(predictor="mat",
           param="rsu") %>% 
    mutate(id=row_number())
  
  beta2_mn<-data.frame(value=as.numeric(MCMC[,'beta2_mn'])) %>%
    mutate(predictor="tap",
           param="mn") %>% 
    mutate(id=row_number())
  beta2_mx<-data.frame(value=as.numeric(MCMC[,'beta2_mx'])) %>%
    mutate(predictor="tap",
           param="mx") %>% 
    mutate(id=row_number())
  beta2_sos<-data.frame(value=as.numeric(MCMC[,'beta2_sos']))%>%
    mutate(predictor="tap",
           param="sos") %>% 
    mutate(id=row_number())
  beta2_eos<-data.frame(value=as.numeric(MCMC[,'beta2_eos']))%>%
    mutate(predictor="tap",
           param="eos") %>% 
    mutate(id=row_number())
  beta2_rsp<-data.frame(value=as.numeric(MCMC[,'beta2_rsp'])) %>%
    mutate(predictor="tap",
           param="rsp") %>% 
    mutate(id=row_number())
  beta2_rau<-data.frame(value=as.numeric(MCMC[,'beta2_rau'])) %>%
    mutate(predictor="tap",
           param="rau") %>% 
    mutate(id=row_number())
  beta2_rsu<-data.frame(value=as.numeric(MCMC[,'beta2_rsu'])) %>%
    mutate(predictor="tap",
           param="rsu") %>% 
    mutate(id=row_number())
  
  df<-bind_rows(beta1_mn, beta1_mx, beta1_sos, beta1_eos,beta1_rsp, beta1_rau,
                beta2_mn, beta2_mx, beta2_sos, beta2_eos,beta2_rsp, beta2_rau) %>% 
    mutate(param=factor(param, levels=c("mn","mx", "sos", "eos", "rsp", "rau", "rsu")))
  
  return(df)
}

df_beta<-get_betas(runMCMC_samples_view)

ggplot(df_beta)+
  geom_histogram(aes(x=value))+
  facet_grid(cols=vars(param), rows=vars(predictor), scale="free")+
  geom_vline(xintercept=0, col="red")+
  theme_classic()

ggplot(df_beta)+
  geom_line(aes(x=id, y=value))+
  facet_grid(cols=vars(param), rows=vars(predictor), scale="free")+
  geom_hline(yintercept=0, col="red")+
  theme_classic()
