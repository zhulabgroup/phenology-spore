source("code/14a_bayes_utils.R")

df_pheno_model_all_years<-df_pheno %>% 
  # filter(location %in% site_list) %>% 
  # mutate(location = factor(location, levels=site_list_order)) %>% 
  drop_na() %>% 
  mutate(level_site=as.integer(factor(location)))

site_list_all<-df_pheno_model_all_years %>% pull(location) %>% unique()
ns<-df_pheno_model_all_years %>% pull(level_site) %>% unique() %>% length()

df_pheno_model_all_years_new<-df_params<-vector(mode="list")

for (site in site_list_all) {
  df_pheno_model<-df_pheno_model_all_years %>% 
    filter(location==site) %>% 
    mutate(level_year=as.integer(factor(year))) %>% 
    mutate(date=as.Date(str_c(year %>% as.character(), "-01-01"))+doy-1)
  
  nt<-df_pheno_model %>% pull(level_year) %>% unique() %>% length()
  
  source("code/14c_bayes_daymet.R")
  
  level_df<-df_pheno_model %>%
    distinct(level_site, level_year) %>%
    mutate(level_siteyear=row_number())
  
  df_pheno_model<-df_pheno_model %>% 
    left_join(level_df, by=c("level_site", "level_year"))
  
  N<-nrow(df_pheno_model)
  
  ########################
  # Design model
  ########################
  mn_min = log(quantile(df_pheno_model_all_years$count, 0.5)) - 2*(log(quantile(df_pheno_model_all_years$count, 0.5)) -log(quantile(df_pheno_model_all_years$count, 0.05)) )
  mn_max = log(quantile(df_pheno_model_all_years$count, 0.5)) 
  mx_min = log(quantile(df_pheno_model_all_years$count, 0.5)) 
  mx_max = log(quantile(df_pheno_model_all_years$count, 0.5)) + 2*(log(quantile(df_pheno_model_all_years$count, 0.95)) -log(quantile(df_pheno_model_all_years$count, 0.5)) )
  sos_min=0
  sos_max=183
  eos_min=183
  eos_max=365
  rsp_min=30/6
  rsp_max=180/6
  rau_min=30/6
  rau_max=180/6
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
    
    # sig_t_mn ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_mx ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_sos ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_eos ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_rsp ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_rau ~ dinvgamma(shape=0.1, scale=0.1)
    # sig_t_rsu ~ dinvgamma(shape=0.1, scale=0.1)
    sig_t_mn ~ dgamma(shape=1, scale=0.00001)
    sig_t_mx ~ dgamma(shape=1, scale=0.00001)
    sig_t_sos ~ dgamma(shape=1, scale=0.00001)
    sig_t_eos ~ dgamma(shape=1, scale=0.00001)
    sig_t_rsp ~ dgamma(shape=1, scale=0.00001)
    sig_t_rau ~ dgamma(shape=1, scale=0.00001)
    sig_t_rsu ~ dgamma(shape=1, scale=0.00001)
    
    for (t in 1:(nt)) {
      ep_mn_t[t] ~ dnorm (mean = 0, var = sig_t_mn)
      ep_mx_t[t] ~ dnorm (mean = 0, var = sig_t_mx)
      ep_sos_t[t] ~ dnorm (mean = 0, var = sig_t_sos)
      ep_eos_t[t] ~ dnorm (mean = 0, var = sig_t_eos)
      ep_rsp_t[t] ~ dnorm (mean = 0, var = sig_t_rsp)
      ep_rau_t[t] ~ dnorm (mean = 0, var = sig_t_rau)
      ep_rsu_t[t] ~ dnorm (mean = 0, var = sig_t_rsu)
    }
    
    for (i in 1:N) {
      logit (t_mn[i]) <-mu_mn+ep_mn_t[year[i]]
      logit (t_mx[i]) <-mu_mx+ep_mx_t[year[i]]
      logit (t_sos[i]) <-mu_sos+ep_sos_t[year[i]]
      logit (t_eos[i]) <-mu_eos+ep_eos_t[year[i]]
      logit (t_rsp[i]) <-mu_rsp+ep_rsp_t[year[i]]
      logit (t_rau[i]) <-mu_rau+ep_rau_t[year[i]]
      logit (t_rsu[i]) <-mu_rsu+ep_rsu_t[year[i]]
      
      mn[i] <- t_mn[i] *(mn_max-mn_min) + mn_min
      mx[i] <- t_mx[i] *(mx_max-mx_min) + mx_min
      sos[i] <- t_sos[i] *(sos_max-sos_min) + sos_min
      eos[i] <- t_eos[i] *(eos_max-eos_min) + eos_min
      rsp[i] <- t_rsp[i] *(rsp_max-rsp_min) + rsp_min
      rau[i] <- t_rau[i] *(rau_max-rau_min) + rau_min
      rsu[i] <- t_rsu[i] *(rsu_max-rsu_min) + rsu_min
      
      log(lambda[i])<-mn[i] + ((mx[i]-mn[i]) + rsu[i] * doy[i]) *(1/(1+exp((sos[i]-doy[i])/(rsp[i]*(mx[i]-mn[i]))))-1/(1+exp((eos[i]-doy[i])/(rau[i]*(mx[i]-mn[i])))))
      
      y[i] ~dpois(lambda[i])
    }
  })
  
  Data <- list(
    y = round(df_pheno_model$count),
    year = df_pheno_model$level_year,
    doy = df_pheno_model$doy
  )
  
  Dimensions <- list(
    ep_mn_t=nt+1, # cos some sites only have1 year of data. returns error in that case.
    ep_mx_t=nt+1,
    ep_sos_t=nt+1,
    ep_eos_t=nt+1,
    ep_rsp_t=nt+1,
    ep_rau_t=nt+1,
    ep_rsu_t=nt+1
  )
  
  Inits <- list(
    mu_mn =0,
    mu_mx =0,
    mu_sos =0,
    mu_eos =0,
    mu_rsp =0,
    mu_rau=0,
    mu_rsu=0,
    
    sig_t_mn =0.00001,
    sig_t_mx =0.00001,
    sig_t_sos  =0.00001,
    sig_t_eos =0.00001,
    sig_t_rsp  =0.00001,
    sig_t_rau  =0.00001,
    sig_t_rsu  =0.00001,
    
    ep_mn_t=rep(0, nt+1),
    ep_mx_t=rep(0,nt+1),
    ep_sos_t=rep(0,nt+1),
    ep_eos_t=rep(0,nt+1),
    ep_rsp_t=rep(0,nt+1),
    ep_rau_t=rep(0,nt+1),
    ep_rsu_t=rep(0,nt+1)
  )
  
  Constants <-list(nt=nt,
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
  )
  
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
  MCMCconfiguration$addMonitors("ep_mn_t[]",
                                "ep_mx_t[]",
                                "ep_sos_t[]",
                                "ep_eos_t[]",
                                "ep_rsp_t[]",
                                "ep_rau_t[]",
                                "ep_rsu_t[]"
  )
  mMCMC <- buildMCMC(MCMCconfiguration)
  CModel <- compileNimble(Model, resetFunctions = T)
  Cmcmc <- compileNimble(mMCMC, project=CModel, resetFunctions = T)
  
  # set.seed(1)
  runMCMC_samples <- runMCMC(Cmcmc, nburnin = 200, niter = 1000, setSeed = 1)
  dir.create(str_c(.path$out_bayes_model, taxa, "/"), recursive = T)
  write_csv(as.data.frame(runMCMC_samples), str_c(.path$out_bayes_model, taxa, "/",site,"_mcmc.csv"))
  
  ########################
  # Get parameters
  ########################
  runMCMC_samples_view <- as.matrix(read_csv(str_c(.path$out_bayes_model, taxa, "/",site,"_mcmc.csv")))
  params<-apply(runMCMC_samples_view,2,median)
  
  df_pheno_model<-df_pheno_model %>% 
    mutate(y_pred=dl(doy,level_year,params))
  
  df_pheno_model_all_years_new[[site]]<-df_pheno_model
  
  df_params[[site]]<-df_pheno_model %>% 
    distinct(level_site, level_year, level_siteyear, mat, tap, mvp, location, year) %>% 
    left_join(get_params(params, nt), by=c("level_year"))   %>% 
    gather(key="param", value="value", -level_site, -level_year, -level_siteyear, -mat, -tap,-mvp,-location, -year) %>% 
    mutate(param=factor(param, levels=c("mn", "mx", "sos", "eos", "rsp", "rau", "rsu"))) %>% 
    mutate(level_site=factor(level_site))
  
  print(site)
}

df_params<-bind_rows(df_params)
df_pheno_model_all_years_new<-bind_rows(df_pheno_model_all_years_new)

write_rds(df_params, str_c(.path$out_bayes_model, taxa, "/", "params.rds"))
write_rds(df_pheno_model_all_years_new, str_c(.path$out_bayes_model, taxa, "/", "fit.rds"))
