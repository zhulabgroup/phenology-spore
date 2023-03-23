dl<-function (doy,year,params) {
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  ep_mn_t<-params[paste0('ep_mn_t[',year,']')] 
  ep_mx_t<-params[paste0('ep_mx_t[',year,']')] 
  ep_sos_t<-params[paste0('ep_sos_t[',year,']')] 
  ep_eos_t<-params[paste0('ep_eos_t[',year,']')] 
  ep_rsp_t<-params[paste0('ep_rsp_t[',year,']')] 
  ep_rau_t<-params[paste0('ep_rau_t[',year,']')] 
  ep_rsu_t<-params[paste0('ep_rsu_t[',year,']')] 
  
  logit_t_mn <-mu_mn+ep_mn_t
  logit_t_mx <-mu_mx+ep_mx_t
  logit_t_sos <-mu_sos+ep_sos_t
  logit_t_eos <-mu_eos+ep_eos_t
  logit_t_rsp <-mu_rsp+ep_rsp_t
  logit_t_rau <-mu_rau+ep_rau_t
  logit_t_rsu <-mu_rsu+ep_rsu_t
  
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

get_params<-function(params, nt){
  mu_mn<-params['mu_mn']
  mu_mx<-params['mu_mx']
  mu_sos<-params['mu_sos']
  mu_eos<-params['mu_eos']
  mu_rsp<-params['mu_rsp']
  mu_rau<-params['mu_rau']
  mu_rsu<-params['mu_rsu']
  
  ep_mn_t<-rep(0, nt)
  ep_mx_t<-rep(0, nt)
  ep_sos_t<-rep(0, nt)
  ep_eos_t<-rep(0, nt)
  ep_rsp_t<-rep(0, nt)
  ep_rau_t<-rep(0, nt)
  ep_rsu_t<-rep(0, nt)
  for (t in 1:nt) {
    ep_mn_t[t]<-params[paste0('ep_mn_t[',t,']')] 
    ep_mx_t[t]<-params[paste0('ep_mx_t[',t,']')] 
    ep_sos_t[t]<-params[paste0('ep_sos_t[',t,']')] 
    ep_eos_t[t]<-params[paste0('ep_eos_t[',t,']')] 
    ep_rsp_t[t]<-params[paste0('ep_rsp_t[',t,']')] 
    ep_rau_t[t]<-params[paste0('ep_rau_t[',t,']')] 
    ep_rsu_t[t]<-params[paste0('ep_rsu_t[',t,']')] 
  }
  ep_mn_t[is.na(ep_mn_t)]<-0
  ep_mx_t[is.na(ep_mx_t)]<-0
  ep_sos_t[is.na(ep_sos_t)]<-0
  ep_eos_t[is.na(ep_eos_t)]<-0
  ep_rsp_t[is.na(ep_rsp_t)]<-0
  ep_rau_t[is.na(ep_rau_t)]<-0
  ep_rsu_t[is.na(ep_rsu_t)]<-0
  
  params_all<-vector(mode="list", length=nt)
  
  for (t in 1:nt) {
    logit_t_mn <-mu_mn+ep_mn_t[t]
    logit_t_mx <-mu_mx+ep_mx_t[t]
    logit_t_sos <-mu_sos+ep_sos_t[t]
    logit_t_eos <-mu_eos+ep_eos_t[t]
    logit_t_rsp <-mu_rsp+ep_rsp_t[t]
    logit_t_rau <-mu_rau+ep_rau_t[t]
    logit_t_rsu <-mu_rsu+ep_rsu_t[t]
    
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
    
    params_all[[t]]<-data.frame(level_year=t,
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