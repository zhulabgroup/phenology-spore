parameters_df<-read_rds("~/spore_phenology/data/parameters.rds")
p_load(rjags)
p_load(coda)
p_load(runjags)
p_load(MASS)
p_load(MCMCpack)

#log(peak_con+1)~year
##read the data
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
  mutate(log_peak=log(peak_con+1)) %>% 
  mutate(peak_doy=format(peak_date,"%j") %>% as.integer())
log_peak<-data_peak$log_peak
year<-data_peak$year
N<-length(log_peak)
id<-data_peak$id
id_list<-unique(id)
peak_doy<-data_peak$peak_doy
d_peak<-read.jagsdata("~/spore_phenology/code/11_sporedata_peak.R")
##initials, three chains
tau0<-rep(1,3)
tau00<-rep(0.5,3)
tau000<-rep(0.1,3)
inits1<-list(tau = tau0, a = 60, b = 1)
inits2<-list(tau = tau00, a = 0, b = 0)
inits3<-list(tau = tau000, a = -60, b = -1)
##analysis
m_log_peak<-jags.model("~/spore_phenology/code/11_spore_log_peak_year.bug", d_peak, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_log_peak,5000)
parameters_log_peak<-coda.samples(m_log_peak, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_log_peak)
##check results
traceplot(parameters_log_peak[,"a"])
traceplot(parameters_log_peak[,"b"])
plot(parameters_log_peak[,"a"])
plot(parameters_log_peak[,"b"])
##check convergence
gelman.plot(parameters_log_peak[,"a"])
gelman.plot(parameters_log_peak[,"b"])
##predicted values
pred_log_peak<-coda.samples(m_log_peak,c("log_peak.pred"), n.iter=10000, thin = 1)
summary(pred_log_peak)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_log_peak, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_log_peak, n.iter=1000, thin = 1, type = "popt")

#peak_doy~year
##analysis
m_peak_doy<-jags.model("~/spore_phenology/code/11_spore_peak_doy_year.bug", d_peak, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_peak_doy,5000)
parameters_peak_doy<-coda.samples(m_peak_doy, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_peak_doy)
##check results
traceplot(parameters_peak_doy[,"a"])
traceplot(parameters_peak_doy[,"b"])
plot(parameters_peak_doy[,"a"])
plot(parameters_peak_doy[,"b"])
##check convergence
gelman.plot(parameters_peak_doy[,"a"])
gelman.plot(parameters_peak_doy[,"b"])
##predicted values
pred_peak_doy<-coda.samples(m_peak_doy,c("peak_doy.pred"), n.iter=10000, thin = 1)
summary(pred_peak_doy)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_peak_doy, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_peak_doy, n.iter=1000, thin = 1, type = "popt")

#log(integral+1)~year
## read the data
data_integral<-parameters_df %>% 
  filter(!is.na(integral)) %>%
  mutate(log_integral=log(integral+1))
log_integral<-data_integral$log_integral
year<-data_integral$year
N<-length(log_integral)
id<-data_integral$id
id_list<-unique(id)
d_integral<-read.jagsdata("~/spore_phenology/code/11_sporedata_integral.R")
##initials, three chains
tau0<-rep(1,3)
tau00<-rep(0.5,3)
tau000<-rep(0.1,3)
inits1<-list(tau = tau0, a = 10, b = 1)
inits2<-list(tau = tau00, a = 0, b = 0)
inits3<-list(tau = tau000, a = -10, b = -1)
##analysis
m_log_integral<-jags.model("~/spore_phenology/code/11_spore_log_integral_year.bug", d_integral, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_log_integral,5000)
parameters_log_integral<-coda.samples(m_log_integral, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_log_integral)
##check results
traceplot(parameters_log_integral[,"a"])
traceplot(parameters_log_integral[,"b"])
plot(parameters_log_integral[,"a"])
plot(parameters_log_integral[,"b"])
##check convergence
gelman.plot(parameters_log_integral[,"a"])
gelman.plot(parameters_log_integral[,"b"])
##predicted values
pred_log_integral<-coda.samples(m_log_integral,c("log_integral.pred"), n.iter=10000, thin = 1)
summary(pred_log_integral)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_log_integral, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_log_integral, n.iter=1000, thin = 1, type = "popt")

#start_doy~year
## read the data
data_season<-parameters_df %>% 
  filter((id==21 & year%in%c(2014,2015,2016,2017)) |
           (id==36 & year!=2018) |
           (id==32) |
           (id==35 & year!=2013 & year!=2019) |
           (id==50 & year%in%c(2012,2013,2014,2015,2016)) |
           (id==49) |
           (id==37)) %>% 
  mutate(start_doy=format(start_date,"%j") %>% as.integer()) %>% 
  mutate(end_doy=format(end_date,"%j") %>% as.integer()) %>% 
  mutate(duration=end_doy-start_doy+1)
start_doy<-data_season$start_doy
end_doy<-data_season$end_doy
duration<-data_season$duration
year<-data_season$year
N<-length(start_doy)
id<-data_season$id
id_list<-unique(id)
d_season<-read.jagsdata("~/spore_phenology/code/11_sporedata_season.R")
##initials, three chains
tau0<-rep(1,3)
tau00<-rep(0.5,3)
tau000<-rep(0.1,3)
inits1<-list(tau = tau0, a = 10, b = 1)
inits2<-list(tau = tau00, a = 0, b = 0)
inits3<-list(tau = tau000, a = -10, b = -1)
##analysis
m_start_doy<-jags.model("~/spore_phenology/code/11_spore_start_doy_year.bug", d_season, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_start_doy,5000)
parameters_start_doy<-coda.samples(m_start_doy, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_start_doy)
##check results
traceplot(parameters_start_doy[,"a"])
traceplot(parameters_start_doy[,"b"])
plot(parameters_start_doy[,"a"])
plot(parameters_start_doy[,"b"])
##check convergence
gelman.plot(parameters_start_doy[,"a"])
gelman.plot(parameters_start_doy[,"b"])
##predicted values
pred_start_doy<-coda.samples(m_start_doy,c("start_doy.pred"), n.iter=10000, thin = 1)
summary(pred_start_doy)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_start_doy, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_start_doy, n.iter=1000, thin = 1, type = "popt")

#start_doy~year
##analysis
m_end_doy<-jags.model("~/spore_phenology/code/11_spore_end_doy_year.bug", d_season, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_end_doy,5000)
parameters_end_doy<-coda.samples(m_end_doy, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_end_doy)
##check results
traceplot(parameters_end_doy[,"a"])
traceplot(parameters_end_doy[,"b"])
plot(parameters_end_doy[,"a"])
plot(parameters_end_doy[,"b"])
##check convergence
gelman.plot(parameters_end_doy[,"a"])
gelman.plot(parameters_end_doy[,"b"])
##predicted values
pred_end_doy<-coda.samples(m_end_doy,c("end_doy.pred"), n.iter=10000, thin = 1)
summary(pred_end_doy)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_end_doy, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_end_doy, n.iter=1000, thin = 1, type = "popt")

#duration~year
##analysis
m_duration<-jags.model("~/spore_phenology/code/11_spore_duration_year.bug", d_season, inits = list(inits1,inits2,inits3), n.chains =3)
update(m_duration,5000)
parameters_duration<-coda.samples(m_duration, c("a","b","alpha","beta","tau"), n.iter = 5000, thin = 1)
summary(parameters_duration)
##check results
traceplot(parameters_duration[,"a"])
traceplot(parameters_duration[,"b"])
plot(parameters_duration[,"a"])
plot(parameters_duration[,"b"])
##check convergence
gelman.plot(parameters_duration[,"a"])
gelman.plot(parameters_duration[,"b"])
##predicted values
pred_duration<-coda.samples(m_duration,c("duration.pred"), n.iter=10000, thin = 1)
summary(pred_duration)
##Model selection, DIC, Deviance Information Criterion
###penalty type pD or popt, only when more than one chain are running
dic.samples(m_duration, n.iter=1000, thin = 1, type = "pD")
dic.samples(m_duration, n.iter=1000, thin = 1, type = "popt")