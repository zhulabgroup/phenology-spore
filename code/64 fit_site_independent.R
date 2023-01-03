library(nimble, warn.conflicts = FALSE)
p_load(daymetr)
p_load(lubridate)
p_load(gridExtra)
########################
# Design model
########################
df_pheno_model_all_years<-df_pheno_subset %>% 
  drop_na() %>% 
  # filter(id==35|id==23|id==49)%>%
  mutate(level_site=as.integer(factor(id)))

ns<-df_pheno_model_all_years %>% pull(level_site) %>% unique() %>% length()

df_pheno_model_all_years_new<-df_params<-vector(mode="list", length=ns)
dir.create(paste0("./nab/site/"))
for (s in 1:ns) {
  df_pheno_model<-df_pheno_model_all_years %>% 
    filter(level_site==s) %>% 
    mutate(level_year=as.integer(factor(year))) 
  
  nt<-df_pheno_model %>% pull(level_year) %>% unique() %>% length()
  
    meta<-df_pheno_model %>% 
      slice(1) %>% 
      dplyr::select(lat, lon, station)
    df_daymet<- download_daymet(site = meta$station,
                                      lat = meta$lat,
                                      lon = meta$lon,
                                      start = year(min(df_pheno_model$Date)),
                                      end = min(year(max(df_pheno_model$Date))+1, 2020),
                                      internal = TRUE,
                                      simplify = TRUE) %>% 
      
      mutate(level_site=s) %>%
      filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.","prcp..mm.day.")) %>% 
      spread(key = "measurement", value="value") %>% 
      rename(prcp=`prcp..mm.day.`,
             tmax=`tmax..deg.c.`,
             tmin=`tmin..deg.c.`) %>% 
      mutate(Date=as.Date(yday, origin = paste0(year,"-01-01"))-1) %>% 
      mutate (temp=(tmax+tmin/2)) %>% 
      dplyr::select(level_site, Date, temp, prcp)
  
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
  
  # df_pheno_model<-df_pheno_model%>% 
    # mutate(tap=(tap-mean(df_pheno_model$tap))/(max(df_pheno_model$tap)-min(df_pheno_model$tap))) %>% 
    # mutate(mat=(mat-mean(df_pheno_model$mat))/(max(df_pheno_model$mat)-min(df_pheno_model$mat))) %>% 
    # mutate(lat=(lat-mean(df_pheno_model$lat))/(max(df_pheno_model$lat)-min(df_pheno_model$lat))) %>% 
    # mutate(year=(year-mean(df_pheno_model$year))/(max(df_pheno_model$year)-min(df_pheno_model$year)))
  
  level_df<-df_pheno_model %>%
    distinct(level_site, level_year) %>%
    mutate(level_siteyear=row_number())
  
  df_pheno_model<-df_pheno_model %>% 
    left_join(level_df, by=c("level_site", "level_year"))
  
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
    
    for (t in 1:(nt+1)) {
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
      
      log(lambda[i])<-mn[i] + ((mx[i]-mn[i]) + rsu[i] * doy[i]) *(1/(1+exp((sos[i]-doy[i])/rsp[i]))-1/(1+exp((eos[i]-doy[i])/rau[i])))
      
      y[i] ~dpois(lambda[i])
    }
  })
  
  Data <- list(
    y = round(df_pheno_model$count_agg),
    year = df_pheno_model$level_year,
    doy = df_pheno_model$doy#,
    # siteyear = df_pheno_model$level_siteyear,
    # lat = df_pheno_model$lat,
    # mat = df_pheno_model$mat,
    # tap = df_pheno_model$tap,
    # site=level_df$level_site
  )
  
  Dimensions <- list(
    ep_mn_t=nt+1,
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
    
    ep_mn_t=rep(0, nt),
    ep_mx_t=rep(0,nt),
    ep_sos_t=rep(0,nt),
    ep_eos_t=rep(0,nt),
    ep_rsp_t=rep(0,nt),
    ep_rau_t=rep(0,nt),
    ep_rsu_t=rep(0,nt)
  )
  
  Constants <-list(#ns=ns,
    nt=nt,
    # nst=nst,
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
  MCMCconfiguration$addMonitors("ep_mn_t[]",
                                "ep_mx_t[]",
                                "ep_sos_t[]",
                                "ep_eos_t[]",
                                "ep_rsp_t[]",
                                "ep_rau_t[]",
                                "ep_rsu_t[]"
  )
  # MCMCconfiguration
  mMCMC <- buildMCMC(MCMCconfiguration)
  CModel <- compileNimble(Model, resetFunctions = T)
  Cmcmc <- compileNimble(mMCMC, project=CModel, resetFunctions = T)
  
  runMCMC_samples <- runMCMC(Cmcmc, nburnin = 200, niter = 1000)
  write_csv(as.data.frame(runMCMC_samples), paste0("./nab/site/",s,"_mcmc.csv"))
  
  ###
  runMCMC_samples_view <- as.matrix(read_csv(paste0("./nab/site/",s,"_mcmc.csv")))# [4001:8000,]
  params<-apply(runMCMC_samples_view,2,median)
  
  df_pheno_model<-df_pheno_model %>% 
    # mutate(y_init=dl(doy, level_year, params_init)) %>%
    mutate(y_pred=dl(doy,level_year,params))
  
  df_pheno_model_all_years_new[[s]]<-df_pheno_model
  
  df_params[[s]]<-df_pheno_model %>% 
    distinct(level_site, level_year, level_siteyear, mat, tap, id, station, year, lon, lat) %>% 
    left_join(get_params(params, nt), by=c("level_year"))   %>% 
    gather(key="param", value="value", -level_site, -level_year, -level_siteyear, -mat, -tap,-id, -station, -year, -lon, -lat) %>% 
    mutate(param=factor(param, levels=c("mn", "mx", "sos", "eos", "rsp", "rau", "rsu"))) %>% 
    mutate(level_site=factor(level_site))
}

df_params<-bind_rows(df_params)
df_pheno_model_all_years_new<-bind_rows(df_pheno_model_all_years_new)

p<-ggplot(df_pheno_model_all_years_new)+
  geom_line(aes(x=doy, y=log(count_agg), col=year,group=year),lty=2)+
  geom_line(aes(x=doy, y=log(y_pred), col=year,group=year))+
  theme_classic()+
  scale_color_viridis_c()+
  # facet_wrap(.~id*station)+
  facet_grid(row=vars(id), col=vars(year), scale="free_y")+
  guides(col=F)
cairo_pdf(paste0("./nab/site/fit curve.pdf"))
print(p)
dev.off()

p<-ggplot(df_pheno_model_all_years_new)+
  geom_point(aes(x=count_agg, y = y_pred, group=year, col=year), alpha=0.1)+
  geom_abline(intercept = 0, slope=1, col="red", lty=2)+
  # coord_equal()+
  theme_classic()+
  facet_wrap(.~id*station,scale="free")+
  scale_color_viridis_c()
cairo_pdf(paste0("./nab/site/goodness of fit.pdf"))
print(p)
dev.off()

p<-ggplot(df_params)+
  geom_line(aes(x=year, y=value, col=level_site, group=level_site))+
  geom_point(aes(x=year, y=value, col=level_site, group=level_site))+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y")
cairo_pdf(paste0("./nab/site/param vs site and year.pdf"))
print(p)
dev.off()

df_params<-df_params %>% 
  mutate(t_value=case_when(param=="mn"~(value-mn_min)/(mn_max-mn_min),
                         param=="mx"~(value-mx_min)/(mx_max-mx_min),
                         param=="sos"~(value-sos_min)/(sos_max-sos_min),
                         param=="eos"~(value-eos_min)/(eos_max-eos_min),
                         param=="rsp"~(value-rsp_min)/(rsp_max-rsp_min),
                         param=="rau"~(value-rau_min)/(rau_max-rau_min),
                         param=="rsu"~(value-rsu_min)/(rsu_max-rsu_min),
  ))
p_mat<-ggplot(df_params)+
  geom_point(aes(x=mat, y=t_value, col=level_site, group=level_site))+
  geom_smooth(aes(x=mat, y=t_value),method="glm", method.args = list(family = "quasibinomial"))+
  # geom_smooth(aes(x=mat, y=value),method=lm)+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y", ncol=1)+
  guides(col=F)

p_tap<-ggplot(df_params)+
  geom_point(aes(x=tap, y=t_value, col=level_site, group=level_site))+
  geom_smooth(aes(x=tap, y=t_value),method="glm", method.args = list(family = "quasibinomial"))+
  # geom_smooth(aes(x=tap, y=value),method=lm)+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y", ncol=1)+
  guides(col=F)

cairo_pdf(paste0("./nab/site/param vs mat and tap.pdf"))
print(grid.arrange(p_mat, p_tap, nrow=1))
dev.off()

cor(df_pheno_model_all_years_new$count_agg, df_pheno_model_all_years_new$y_pred)

summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="mn")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="mx")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="sos")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="eos")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="rsp")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="rau")))
summary(lm(logit(t_value)~mat+tap, data=df_params %>% filter(param=="rsu")))

write_rds(df_pheno_model_all_years_new, file = "/data/ZHULAB/phenology/nab/nab_dat_modeled.rds")
write_rds(df_params, file = "/data/ZHULAB/phenology/nab/nab_dat_params.rds")
