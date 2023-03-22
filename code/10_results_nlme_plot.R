parameters_df<-read_rds("~/spore_phenology/data/parameters.rds")

p_load(lme4)
p_load(blme)
p_load(nlme)
p_load(ggeffects)

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
  mutate(id=as.character(id)) %>% 
  mutate(log_peak=log(peak_con+1)) %>% 
  rename("peak_doy"="doy")
# model_lmer1 <- lmer(log_peak ~ year + (1 | id), data = data_peak)
# summary(model_lmer1)
# model_lmer2 <- lmer(log_peak ~ year + (year | id), data = data_peak, REML = TRUE)
# summary(model_lmer2)
# model_nlme1 <- lme(log_peak ~ year, data = data_peak, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_log_peak <- lme(log_peak ~ year, data = data_peak, random = ~year | id)
summary(nlme_log_peak)
##read fitted data and variance
log_peak_fit<-data_peak %>% 
  mutate(log_peak_fit.fixed=nlme_log_peak$fitted[,1],
         log_peak_fit.random=nlme_log_peak$fitted[,2]) %>% 
  as_tibble()
log_peak_fix_var<-ggpredict(nlme_log_peak, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=log_peak_fit,aes (x=year, y=log_peak, group=id, col=id))+
  geom_path(data=log_peak_fit,aes (x=year, y=log_peak_fit.random, group=id, col=id))+
  geom_path(data=log_peak_fit,aes (x=year, y=log_peak_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=log_peak_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2009, 2019, by=1))+
  theme_classic()

#peak_doy~year
# model_lmer1 <- lmer(peak_doy ~ year + (1 | id), data = data_peak)
# model_nlme <- lme(log_con ~ year, data = data_peak, random = ~1 | id)
# summary(model_lmer1)
# model_nlme1 <- lme(peak_doy ~ year, data = data_peak, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_peak_doy <- lme(peak_doy ~ year, data = data_peak, random = ~year | id)
summary(nlme_peak_doy)
##read fitted data and variance
peak_doy_fit<-data_peak %>% 
  mutate(peak_doy_fit.fixed=nlme_peak_doy$fitted[,1],
         peak_doy_fit.random=nlme_peak_doy$fitted[,2]) %>% 
  as_tibble()
peak_doy_fix_var<-ggpredict(nlme_peak_doy, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=peak_doy_fit,aes (x=year, y=peak_doy, group=id, col=id))+
  geom_path(data=peak_doy_fit,aes (x=year, y=peak_doy_fit.random, group=id, col=id))+
  geom_path(data=peak_doy_fit,aes (x=year, y=peak_doy_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=peak_doy_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2009, 2019, by=1))+
  theme_classic()

#log(integral+1)~year
##read the data
data_integral<-parameters_df %>% 
  filter(!is.na(integral)) %>%
  mutate(log_integral=log(integral+1)) %>% 
  mutate(id=as.character(id))
# model_lmer1 <- lmer(log_integral ~ year + (1 | id), data = data_integral)
# summary(model_lmer1)
# model_nlme1 <- lme(log_integral ~ year, data = data_integral, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_log_integral <- lme(log_integral ~ year, data = data_integral, random = ~year | id)
summary(nlme_log_integral)
##read fitted data and variance
log_integral_fit<-data_integral %>% 
  mutate(log_integral_fit.fixed=nlme_log_integral$fitted[,1],
         log_integral_fit.random=nlme_log_integral$fitted[,2]) %>% 
  as_tibble()
log_integral_fix_var<-ggpredict(nlme_log_integral, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=log_integral_fit,aes (x=year, y=log_integral, group=id, col=id))+
  geom_path(data=log_integral_fit,aes (x=year, y=log_integral_fit.random, group=id, col=id))+
  geom_path(data=log_integral_fit,aes (x=year, y=log_integral_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=log_integral_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2007, 2019, by=1))+
  theme_classic()

#start_doy~year
##read the data
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
  mutate(duration=end_doy-start_doy+1) %>% 
  mutate(id=as.character(id))
# model_lmer1 <- lmer(start_doy ~ year + (1 | id), data = data_season)
# summary(model_lmer1)
# model_nlme1 <- lme(start_doy ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_start_doy <- lme(start_doy ~ year, data = data_season, random = ~year | id)
summary(nlme_start_doy)
##read fitted data and variance
start_doy_fit<-data_season %>% 
  mutate(start_doy_fit.fixed=nlme_start_doy$fitted[,1],
         start_doy_fit.random=nlme_start_doy$fitted[,2]) %>% 
  as_tibble()
start_doy_fix_var<-ggpredict(nlme_start_doy, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=start_doy_fit,aes (x=year, y=start_doy, group=id, col=id))+
  geom_path(data=start_doy_fit,aes (x=year, y=start_doy_fit.random, group=id, col=id))+
  geom_path(data=start_doy_fit,aes (x=year, y=start_doy_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=start_doy_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2007, 2019, by=1))+
  theme_classic()

#end_doy~year
# model_lmer1 <- lmer(end_doy ~ year + (1 | id), data = data_season)
# model_nlme1 <- lme(end_doy ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_end_doy <- lme(end_doy ~ year, data = data_season, random = ~year | id, control=lmeControl(opt="optim"))
summary(nlme_end_doy)
##read fitted data and variance
end_doy_fit<-data_season %>% 
  mutate(end_doy_fit.fixed=nlme_end_doy$fitted[,1],
         end_doy_fit.random=nlme_end_doy$fitted[,2]) %>% 
  as_tibble()
end_doy_fix_var<-ggpredict(nlme_end_doy, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=end_doy_fit,aes (x=year, y=end_doy, group=id, col=id))+
  geom_path(data=end_doy_fit,aes (x=year, y=end_doy_fit.random, group=id, col=id))+
  geom_path(data=end_doy_fit,aes (x=year, y=end_doy_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=end_doy_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2007, 2019, by=1))+
  theme_classic()

#duration~year
# model_lmer1 <- lmer(duration ~ year + (1 | id), data = data_season)
# summary(model_lmer1)
# model_nlme1 <- lme(duration ~ year, data = data_season, random = ~1 | id)
# summary(model_nlme1)
##fit nlme
nlme_duration <- lme(duration ~ year, data = data_season, random = ~year | id)
summary(nlme_duration)
##read fitted data and variance
duration_fit<-data_season %>% 
  mutate(duration_fit.fixed=nlme_duration$fitted[,1],
         duration_fit.random=nlme_duration$fitted[,2]) %>% 
  as_tibble()
duration_fix_var<-ggpredict(nlme_duration, "year", type = "re") %>% 
  as_tibble()
##plot
ggplot() +
  geom_jitter(data=duration_fit,aes (x=year, y=duration, group=id, col=id))+
  geom_path(data=duration_fit,aes (x=year, y=duration_fit.random, group=id, col=id))+
  geom_path(data=duration_fit,aes (x=year, y=duration_fit.fixed), col="black", linewidth=1)+
  geom_ribbon(data=duration_fix_var,aes (x=x, ymin=conf.low, ymax=conf.high), col="gray",  alpha=0.5)+
  scale_x_continuous(breaks=seq(2007, 2019, by=1))+
  theme_classic()