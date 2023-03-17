parameters_df<-read_rds("~/spore_phenology/data/parameters.rds")

###lmer
p_load(lme4)
p_load(blme)
p_load(nlme)
#log(peak_con+1) ~ year
data_peak<-parameters_df %>% 
  filter((id==5 & year%in%c(2013,2016,2017)) | 
           (id==43 & year%in%c(2012,2013,2014)) |
           (id==21 & year!=2018) |
           (id==36) |
           (id==32) |
           (id==19 & year%in%c(2011,2012,2013,2014,2015,2016,2017)) |
           (id==17) |
           (id==35 & year%in%c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)) |
           (id==41) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016,2017)) |
           (id==49) |
           (id==37) |
           (id==48 & year%in%c(2009,2010,2011))) %>% 
  dplyr::select(location,id,lat,lon,year,peak_con,peak_date) %>% 
  mutate(log_con=log(peak_con+1)) %>% 
  dplyr::select(id,year,log_con)
model_lmer1 <- lmer(log_con ~ year + (1 | id), data = data_peak)
summary(model_lmer1)
model_lmer2 <- lmer(log_con ~ year + (year | id), data = data_peak, REML = TRUE)
summary(model_lmer2)
model_nlme1 <- lme(log_con ~ year, data = data_peak, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(log_con ~ year, data = data_peak, random = ~1+year | id)
summary(model_nlme2)

#peak_doy ~ year
data_peak<-parameters_df %>% 
  filter((id==5 & year%in%c(2013,2016,2017)) | 
           (id==43 & year%in%c(2012,2013,2014)) |
           (id==21 & year!=2018) |
           (id==36) |
           (id==32) |
           (id==19 & year%in%c(2011,2012,2013,2014,2015,2016,2017)) |
           (id==17) |
           (id==35 & year%in%c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)) |
           (id==41) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016,2017)) |
           (id==49) |
           (id==37) |
           (id==48 & year%in%c(2009,2010,2011))) %>% 
  dplyr::select(location,id,lat,lon,year,peak_con,peak_date) %>% 
  mutate(peak_doy=format(peak_date,"%j")%>% as.integer()) %>% 
  dplyr::select(id,year,peak_doy)
model_lmer1 <- lmer(peak_doy ~ year + (1 | id), data = data_peak)
model_nlme <- lme(log_con ~ year, data = data_peak, random = ~1 | id)
summary(model_lmer1)
model_nlme1 <- lme(peak_doy ~ year, data = data_peak, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(peak_doy ~ year, data = data_peak, random = ~year | id)
summary(model_nlme2)
#log(integral+1) ~ year
data_integral<-parameters_df %>% 
  filter(!is.na(integral)) %>%
  mutate(log_integral=log(integral+1)) %>% 
  dplyr::select(id,year,log_integral)
model_lmer1 <- lmer(log_integral ~ year + (1 | id), data = data_integral)
summary(model_lmer1)
model_nlme1 <- lme(log_integral ~ year, data = data_integral, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(log_integral ~ year, data = data_integral, random = ~year | id)
summary(model_nlme2)

#start_doy ~ year
data_season<-parameters_df %>% 
  filter((id==21 & year%in%c(2014,2015,2016,2017)) |
           (id==36 & year!=2018) |
           (id==32) |
           (id==35 & year!=2013 & year!=2019) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016)) |
           (id==49) |
           (id==37)) %>% 
  dplyr::select(location,id,lat,lon,year,start_date,end_date) %>% 
  mutate(start_doy=format(start_date,"%j") %>% as.integer()) %>% 
  mutate(end_doy=format(end_date,"%j") %>% as.integer()) %>% 
  mutate(duration=end_doy-start_doy+1) %>% 
  dplyr::select(id,year,start_doy)
model_lmer1 <- lmer(start_doy ~ year + (1 | id), data = data_season)
summary(model_lmer1)
model_nlme1 <- lme(start_doy ~ year, data = data_season, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(start_doy ~ year, data = data_season, random = ~year | id)
summary(model_nlme2)

#end_doy ~ year
data_season<-parameters_df %>% 
  filter((id==21 & year%in%c(2014,2015,2016,2017)) |
           (id==36 & year!=2018) |
           (id==32) |
           (id==35 & year!=2013 & year!=2019) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016)) |
           (id==49) |
           (id==37)) %>% 
  dplyr::select(location,id,lat,lon,year,start_date,end_date) %>% 
  mutate(start_doy=format(start_date,"%j") %>% as.integer()) %>% 
  mutate(end_doy=format(end_date,"%j") %>% as.integer()) %>% 
  mutate(duration=end_doy-start_doy+1) %>% 
  dplyr::select(id,year,end_doy)
model_lmer1 <- lmer(end_doy ~ year + (1 | id), data = data_season)
model_nlme1 <- lme(end_doy ~ year, data = data_season, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(end_doy ~ year, data = data_season, random = ~year | id, control=lmeControl(opt="optim"))
summary(model_nlme2)
#duration ~ year
data_season<-parameters_df %>% 
  filter((id==21 & year%in%c(2014,2015,2016,2017)) |
           (id==36 & year!=2018) |
           (id==32) |
           (id==35 & year!=2013 & year!=2019) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016)) |
           (id==49) |
           (id==37)) %>% 
  dplyr::select(location,id,lat,lon,year,start_date,end_date) %>% 
  mutate(start_doy=format(start_date,"%j") %>% as.integer()) %>% 
  mutate(end_doy=format(end_date,"%j") %>% as.integer()) %>% 
  mutate(duration=end_doy-start_doy+1) %>% 
  dplyr::select(id,year,duration)
model_lmer1 <- lmer(duration ~ year + (1 | id), data = data_season)
summary(model_lmer1)
model_nlme1 <- lme(duration ~ year, data = data_season, random = ~1 | id)
summary(model_nlme1)
model_nlme2 <- lme(duration ~ year, data = data_season, random = ~year | id)
summary(model_nlme2)



#jags
p_load(rjags)
p_load(coda)
p_load(runjags)
p_load(MASS)
p_load(MCMCpack)
# Define the JAGS model
model_string <- "
model {
  for(i in 1:N){
    log_con[i]~dnorm(mu[i], tau) #likelihood
    log_con.pred[i]~dnorm(mu[i], tau) #predicted
    mu[i]<-alpha[id[i]]+beta*year #process model
  }
  
  #priors
  a~dnorm(0, 0.0001)
  for(i in c_id){
  aplha[i]~dnorm(a,tau_id)
  }
  beta~dnorm(0,0.0001)
  tau~dgamma(0.0001, 0.0001)
  tau_id~dgamma(0.0001, 0.0001)
}
"
# Compile the JAGS model
model <- jags.model(textConnection(model_string), data = list(N = nrow(data_peak), log_con = data_peak$log_con, year = data_peak$year, id = data_peak$id, c_id = unique(data_peak$id)), n.chains = 3)

# Sample from the posterior distribution using MCMC
samples <- coda.samples(model, variable.names = c("beta0", "beta1", "tau", "mu_u", "tau_u", "u"), n.iter = 5000, thin = 5)

# Summarize the posterior distribution
summary(samples)