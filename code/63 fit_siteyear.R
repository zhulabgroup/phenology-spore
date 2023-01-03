library(nimble, warn.conflicts = FALSE)
p_load(daymetr)
p_load(lubridate)
########################
# Design model
########################
df_pheno_model<-df_pheno_subset %>% 
  drop_na() %>% 
  # filter(id==35|id==23|id==49)%>%
  mutate(level_site=as.integer(factor(id))) %>% 
  mutate(level_year=as.integer(factor(year))) 

ns<-df_pheno_model %>% pull(level_site) %>% unique() %>% length()
nt<-df_pheno_model %>% pull(level_year) %>% unique() %>% length()

df_daymet<-vector(mode="list", length=ns)
for ( i in 1:ns) {
  meta<-df_pheno_model %>% 
    filter(level_site==i) %>% 
    slice(1)
  df_daymet[[i]] <- download_daymet(site = meta$station,
                                    lat = meta$lat,
                                    lon = meta$lon,
                                    start = year(min(df_pheno_model$Date)),
                                    end = min(year(max(df_pheno_model$Date)), 2020),
                                    internal = TRUE,
                                    simplify = TRUE) %>% 
    
    mutate(level_site=i) %>%
    filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.","prcp..mm.day.")) %>% 
    spread(key = "measurement", value="value") %>% 
    rename(prcp=`prcp..mm.day.`,
           tmax=`tmax..deg.c.`,
           tmin=`tmin..deg.c.`) %>% 
    mutate(Date=as.Date(yday, origin = paste0(year,"-01-01"))-1) %>% 
    mutate (temp=(tmax+tmin/2)) %>% 
    dplyr::select(level_site, Date, temp, prcp)
}
df_daymet<-bind_rows(df_daymet)

df_daymet_annual<-df_daymet %>% 
  left_join(df_pheno_model %>% distinct(level_site,Date, year), by=c("level_site","Date") )%>% 
  drop_na(year) %>% 
  group_by(level_site,year) %>% 
  summarize(mat=mean(temp),
            tap=sum(prcp)) %>% 
  ungroup()

df_pheno_model<-df_pheno_model %>% 
  left_join(df_daymet_annual, by=c("level_site","year" )) %>% 
  drop_na(tap, mat)

df_pheno_model<-df_pheno_model%>% 
  mutate(tap=(tap-mean(df_pheno_model$tap))/(max(df_pheno_model$tap)-min(df_pheno_model$tap))) %>% 
  mutate(mat=(mat-mean(df_pheno_model$mat))/(max(df_pheno_model$mat)-min(df_pheno_model$mat))) %>% 
  mutate(lat=(lat-mean(df_pheno_model$lat))/(max(df_pheno_model$lat)-min(df_pheno_model$lat))) %>% 
  mutate(year=(year-mean(df_pheno_model$year))/(max(df_pheno_model$year)-min(df_pheno_model$year)))

level_df<-df_pheno_model %>% 
  distinct(level_site, level_year) %>% 
  mutate(level_siteyear=row_number())

df_pheno_model<-df_pheno_model %>% 
  left_join(level_df, by=c("level_site", "level_year"))

nst<-nrow(level_df)
N<-nrow(df_pheno_model)

#####
mn_min = -5
mn_max=5
mx_min = 5
mx_max=15
sos_min=0
sos_max=183
eos_min=183
eos_max=365
rsp_min=1
rsp_max=61
rau_min=1
rau_max=61
rsu_min=-0.01
rsu_max=0.01

Code <- nimbleCode({
  mu_mn ~ dnorm(mean=0, var=1)
  mu_mx ~ dnorm(mean=0, var=1)
  # constraint_data1  ~ dconstraint( mu_mx>= mu_mn )
  mu_sos ~ dnorm(mean=0, var=1)
  mu_eos ~ dnorm(mean=0, var=1)
  # constraint_data2  ~ dconstraint( mu_eos>= mu_sos )
  mu_rsp ~ dnorm(mean=0, var=1)
  mu_rau ~ dnorm(mean=0, var=1)
  mu_rsu ~ dnorm(mean=0, var=1)
  
  sig_s_mn ~ dinvgamma(shape = 10, scale= 10)
  sig_s_mx ~ dinvgamma(shape = 10, scale= 10)
  sig_s_sos ~ dinvgamma(shape = 10, scale= 10)
  sig_s_eos ~ dinvgamma(shape = 10, scale= 10)
  sig_s_rsp ~ dinvgamma(shape = 10, scale= 10)
  sig_s_rau ~ dinvgamma(shape = 10, scale= 10)
  sig_s_rsu ~ dinvgamma(shape = 10, scale= 10)
  
  sig_t_mn ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_mx ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_sos ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_eos ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_rsp ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_rau ~ dinvgamma(shape=0.1, scale=0.1)
  sig_t_rsu ~ dinvgamma(shape=0.1, scale=0.1)
  
  for (s in 1:ns) {
    ep_mn_s [s] ~ dnorm(mean = 0, var = sig_s_mn)
    ep_mx_s [s] ~ dnorm(mean = 0, var = sig_s_mx)
    ep_sos_s [s] ~ dnorm(mean = 0, var = sig_s_sos)
    ep_eos_s [s] ~ dnorm(mean = 0, var = sig_s_eos)
    ep_rsp_s [s] ~ dnorm(mean = 0, var = sig_s_rsp)
    ep_rau_s [s] ~ dnorm(mean = 0, var = sig_s_rau)
    ep_rsu_s [s] ~ dnorm(mean = 0, var = sig_s_rsu)
  }
  
  for (st in 1:nst) {
    ep_mn_s_t[st] ~ dnorm (mean = ep_mn_s [site[st]], var = sig_t_mn)
    ep_mx_s_t[st] ~ dnorm (mean = ep_mx_s [site[st]], var = sig_t_mx)
    ep_sos_s_t[st] ~ dnorm (mean = ep_sos_s [site[st]], var = sig_t_sos)
    ep_eos_s_t[st] ~ dnorm (mean = ep_eos_s [site[st]], var = sig_t_eos)
    ep_rsp_s_t[st] ~ dnorm (mean = ep_rsp_s [site[st]], var = sig_t_rsp)
    ep_rau_s_t[st] ~ dnorm (mean = ep_rau_s [site[st]], var = sig_t_rau)
    ep_rsu_s_t[st] ~ dnorm (mean = ep_rsu_s [site[st]], var = sig_t_rsu)
  }
  
  for (i in 1:N) {
    logit (t_mn[i]) <-mu_mn+ep_mn_s_t[siteyear[i]]
    logit (t_mx[i]) <-mu_mx+ep_mx_s_t[siteyear[i]]
    logit (t_sos[i]) <-mu_sos+ep_sos_s_t[siteyear[i]]
    logit (t_eos[i]) <-mu_eos+ep_eos_s_t[siteyear[i]]
    logit (t_rsp[i]) <-mu_rsp+ep_rsp_s_t[siteyear[i]]
    logit (t_rau[i]) <-mu_rau+ep_rau_s_t[siteyear[i]]
    logit (t_rsu[i]) <-mu_rsu+ep_rsu_s_t[siteyear[i]]
    
    mn[i] <- t_mn[i] *(mn_max-mn_min) + mn_min
    mx[i] <- t_mx[i] *(mx_max-mx_min) + mx_min
    sos[i] <- t_sos[i] *(sos_max-sos_min) + sos_min
    eos[i] <- t_eos[i] *(eos_max-eos_min) + eos_min
    rsp[i] <- t_rsp[i] *(rsp_max-rsp_min) + rsp_min
    rau[i] <- t_rau[i] *(rau_max-rau_min) + rau_min
    rsu[i] <- t_rsu[i] *(rsu_max-rsu_min) + rsu_min
    
    log(lambda[i])<-mn[i] + ((mx[i]-mn[i]) + rsu[i] * doy[i]) *(1/(1+exp((sos[i]-doy[i])/rsp[i]))-1/(1+exp((eos[i]-doy[i])/rau[i])))
    
    y[i] ~dpois(lambda[i])
  }
})

Data <- list(
  y = round(df_pheno_model$count_agg),
  year = df_pheno_model$level_year,
  doy = df_pheno_model$doy,
  siteyear = df_pheno_model$level_siteyear,
  lat = df_pheno_model$lat,
  mat = df_pheno_model$mat,
  tap = df_pheno_model$tap,
  site=level_df$level_site,
  constraint_data1=1,
  constraint_data2=1,
  constraint_data3=rep(1, ns),
  constraint_data4=rep(1, ns),
  constraint_data5=matrix(1, nrow = ns, ncol=nt),
  constraint_data6=matrix(1, nrow = ns, ncol=nt)
)

Dimensions <- list(
  ep_mn_s=ns,
  ep_mx_s=ns,
  ep_sos_s=ns,
  ep_eos_s=ns,
  ep_rsp_s=ns,
  ep_rau_s=ns,
  ep_rsu_s=ns,
  
  ep_mn_s_t=nst,
  ep_mx_s_t=nst,
  ep_sos_s_t=nst,
  ep_eos_s_t=nst,
  ep_rsp_s_t=nst,
  ep_rau_s_t=nst,
  ep_rsu_s_t=nst
)

Inits <- list(
  mu_mn =0,
  mu_mx =0,
  mu_sos =0,
  mu_eos =0,
  mu_rsp =0,
  mu_rau=0,
  mu_rsu=0,
  
  sig_s_mn =0.1,
  sig_s_mx =0.1,
  sig_s_sos  =0.1,
  sig_s_eos =0.1,
  sig_s_rsp  =0.1,
  sig_s_rau  =0.1,
  sig_s_rsu  =0.1,
  
  sig_t_mn =0.01,
  sig_t_mx =0.01,
  sig_t_sos  =0.01,
  sig_t_eos =0.01,
  sig_t_rsp  =0.01,
  sig_t_rau  =0.01,
  sig_t_rsu  =0.01,
  
  ep_mn_s=rep(0, ns),
  ep_mx_s=rep(0,ns),
  ep_sos_s=rep(0,ns),
  ep_eos_s=rep(0,ns),
  ep_rsp_s=rep(0,ns),
  ep_rau_s=rep(0,ns),
  ep_rsu_s=rep(0,ns),
  
  ep_mn_s_t=rep(0, nst),
  ep_mx_s_t=rep(0, nst),
  ep_sos_s_t=rep(0, nst),
  ep_eos_s_t=rep(0, nst),
  ep_rsp_s_t=rep(0, nst),
  ep_rau_s_t=rep(0, nst),
  ep_rsu_s_t=rep(0, nst)
)


Constants <-list(ns=ns,
                 nt=nt,
                 nst=nst,
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
MCMCconfiguration <- configureMCMC(Model,useConjugacy = T,  print=T)
MCMCconfiguration$addMonitors("ep_mn_s_t[]",
                              "ep_mx_s_t[]",
                              "ep_sos_s_t[]",
                              "ep_eos_s_t[]",
                              "ep_rsp_s_t[]",
                              "ep_rau_s_t[]",
                              "ep_rsu_s_t[]"
)
# MCMCconfiguration$removeSampler(c("ep_mn_s_t[]",
#                                   "ep_mx_s_t[]",
#                                   "ep_sos_s_t[]",
#                                   "ep_eos_s_t[]",
#                                   "ep_rsp_s_t[]",
#                                   "ep_rau_s_t[]",
#                                   "ep_rsu_s_t[]"))
# 
# MCMCconfiguration$addSampler(target=c(paste0("ep_mn_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_mx_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_sos_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_eos_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_rsp_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_rau_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
# MCMCconfiguration$addSampler(target=c(paste0("ep_rsu_s_t[",1:nst,"]")),type = "RW_block",
#                              control = list(adaptive = TRUE))
MCMCconfiguration
mMCMC <- buildMCMC(MCMCconfiguration)
CModel <- compileNimble(Model, resetFunctions = T)
Cmcmc <- compileNimble(mMCMC, project=CModel, resetFunctions = T)

Cmcmc$run( niter = 1)
Cmcmc$mvSamples %>% as.matrix()

runMCMC_samples <- runMCMC(Cmcmc, nburnin = 2000, niter = 10000)
write_csv(as.data.frame(runMCMC_samples), "./nab/mcmc.csv")

########################
# Check MCMC samples
########################

runMCMC_samples_view <- as.matrix(read_csv("./nab/mcmc.csv"))# [4001:8000,]
runMCMC_samples_view %>% tail()
i <- 'ep_mn_s_t[6]'
# i=11
plot(runMCMC_samples_view[, i], type = "l", xlab = "iteration", ylab = colnames(runMCMC_samples_view)[i])
plot(density(runMCMC_samples_view[, i]))
median(runMCMC_samples_view[, i])
mean(runMCMC_samples_view[, i])
var(runMCMC_samples_view[, i])

params<-apply(runMCMC_samples_view,2,median)
params


dl<-function (doy,siteyear,params,ns, nt) {
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  ep_mn_s_t<-params[paste0('ep_mn_s_t[',siteyear,']')] 
  ep_mx_s_t<-params[paste0('ep_mx_s_t[',siteyear,']')] 
  ep_sos_s_t<-params[paste0('ep_sos_s_t[',siteyear,']')] 
  ep_eos_s_t<-params[paste0('ep_eos_s_t[',siteyear,']')] 
  ep_rsp_s_t<-params[paste0('ep_rsp_s_t[',siteyear,']')] 
  ep_rau_s_t<-params[paste0('ep_rau_s_t[',siteyear,']')] 
  ep_rsu_s_t<-params[paste0('ep_rsu_s_t[',siteyear,']')] 
  
  logit_t_mn <-mu_mn+ep_mn_s_t
  logit_t_mx <-mu_mx+ep_mx_s_t
  logit_t_sos <-mu_sos+ep_sos_s_t
  logit_t_eos <-mu_eos+ep_eos_s_t
  logit_t_rsp <-mu_rsp+ep_rsp_s_t
  logit_t_rau <-mu_rau+ep_rau_s_t
  logit_t_rsu <-mu_rsu+ep_rsu_s_t
  
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
  
  log_lambda=mn + ((mx-mn) + rsu * doy) *(1/(1+exp((sos-doy)/rsp))-1/(1+exp((eos-doy)/rau)))
  lambda=exp(log_lambda)
  
  return(lambda)
}

params_init<-c(
  mu_mn=Inits$mu_mn,
  mu_mx=Inits$mu_mx,
  mu_sos=Inits$mu_sos,
  mu_eos=Inits$mu_eos,
  mu_rsp=Inits$mu_rsp,
  mu_rau=Inits$mu_rau,
  mu_rsu=Inits$mu_rsu
)

df<-df_pheno_model %>% 
  mutate(y_init=dl(doy, level_siteyear, params_init, ns, nt)) %>%
  mutate(y_pred=dl(doy,level_siteyear,params, ns, nt))

get_params<-function(params, nst){
  
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  ep_mn_s_t<-rep(0, nst)
  ep_mx_s_t<-rep(0, nst)
  ep_sos_s_t<-rep(0, nst)
  ep_eos_s_t<-rep(0, nst)
  ep_rsp_s_t<-rep(0, nst)
  ep_rau_s_t<-rep(0, nst)
  ep_rsu_s_t<-rep(0, nst)
  for (st in 1:nst) {
    ep_mn_s_t[st]<-params[paste0('ep_mn_s_t[',st,']')] 
    ep_mx_s_t[st]<-params[paste0('ep_mx_s_t[',st,']')] 
    ep_sos_s_t[st]<-params[paste0('ep_sos_s_t[',st,']')] 
    ep_eos_s_t[st]<-params[paste0('ep_eos_s_t[',st,']')] 
    ep_rsp_s_t[st]<-params[paste0('ep_rsp_s_t[',st,']')] 
    ep_rau_s_t[st]<-params[paste0('ep_rau_s_t[',st,']')] 
    ep_rsu_s_t[st]<-params[paste0('ep_rsu_s_t[',st,']')] 
  }
  ep_mn_s_t[is.na(ep_mn_s_t)]<-0
  ep_mx_s_t[is.na(ep_mx_s_t)]<-0
  ep_sos_s_t[is.na(ep_sos_s_t)]<-0
  ep_eos_s_t[is.na(ep_eos_s_t)]<-0
  ep_rsp_s_t[is.na(ep_rsp_s_t)]<-0
  ep_rau_s_t[is.na(ep_rau_s_t)]<-0
  ep_rsu_s_t[is.na(ep_rsu_s_t)]<-0
  
  params_all<-vector(mode="list", length=nst)
  
  for (i in 1:nst) {
    st<-level_df$level_siteyear[i]
    logit_t_mn <-mu_mn+ep_mn_s_t[st]#+beta1_mn*mat+beta2_mn*tap
    logit_t_mx <-mu_mx+ep_mx_s_t[st]#+beta1_mx*mat+beta2_mx*tap
    logit_t_sos <-mu_sos+ep_sos_s_t[st]#+beta1_sos*mat+beta2_sos*tap
    logit_t_eos <-mu_eos+ep_eos_s_t[st]#+beta1_eos*mat+beta2_eos*tap
    logit_t_rsp <-mu_rsp+ep_rsp_s_t[st]#+beta1_rsp*mat+beta2_rsp*tap
    logit_t_rau <-mu_rau+ep_rau_s_t[st]#+beta1_rau*mat+beta2_rau*tap
    logit_t_rsu <-mu_rsu+ep_rsu_s_t[st]#+beta1_rsu*mat+beta2_rsu*tap
    
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
    
    params_all[[i]]<-data.frame(level_siteyear=st,
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
  geom_line(data=df,aes(x=doy, y=log(count_agg), col=year,group=year),lty=2)+
  geom_line(data=df,aes(x=doy, y=log(y_pred), col=year,group=year))+
  theme_classic()+
  scale_color_viridis_c()+
  # facet_wrap(.~id*station)+
  facet_grid(row=vars(id), col=vars(year), scale="free_y")

df_params<-df_pheno_model %>% 
  dplyr::select(level_site, level_year, level_siteyear, mat, tap) %>% 
  left_join(get_params(params, nst), by=c("level_siteyear"))   %>% 
  gather(key="param", value="value", -level_site, -level_year, -level_siteyear, -mat, -tap) %>% 
  mutate(param=factor(param, levels=c("mn", "mx", "sos", "eos", "rsp", "rau", "rsu"))) %>% 
  mutate(level_site=factor(level_site))
get_params(params_init, nst)

ggplot(df_params)+
  geom_line(aes(x=level_year, y=value, col=level_site, group=level_site))+
  geom_point(aes(x=level_year, y=value, col=level_site, group=level_site))+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y")

ggplot(df_params)+
  geom_point(aes(x=mat, y=value, col=level_site, group=level_site))+
  geom_smooth(aes(x=mat, y=value, col=level_site, group=level_site), method = lm, se=F)+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y")

ggplot(df)+
  geom_point(aes(x=count_agg, y = y_pred, group=level_site, col=level_site), alpha=0.1)+
  geom_abline(intercept = 0, slope=1, col="red", lty=2)+
  # coord_equal()+
  theme_classic()+
  facet_wrap(.~id*station,scale="free")
cor(df$count_agg, df$y_pred)

