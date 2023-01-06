library(tidyverse)
library(RcppRoll)
library(scales)

taxa_df<-read_csv("./nab/data/nab_and_inat_taxa.csv")
df_nab<- read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds")

meta_df<-df_nab %>% 
  filter(taxa=="Total Spore Count") %>% 
  group_by(taxa, station, location, lat, lon, id) %>% 
  drop_na(count) %>% 
  filter(count>0) %>% 
  summarise(mindate=min(Date),
            maxdate=max(Date),
            n=n()) %>% 
  mutate(range=maxdate-mindate) %>%
  ungroup() %>% 
  arrange(desc(n))

###
df_comm_all<-df_nab %>% 
  # filter(taxa!="Total Spore Count") %>% 
  rename(taxa_raw=taxa) %>% 
  left_join(taxa_df, by="taxa_raw") %>% 
  mutate(family=case_when(taxa_raw=="Total Spore Count"~"Total",
                          TRUE~family)) %>% 
  mutate(genus=case_when(taxa_raw=="Total Spore Count"~"Total",
                          TRUE~genus)) %>% 
  filter(kingdom!="Viridiplantae"|is.na(kingdom)) %>% 
  group_by(taxa_raw, taxa_clean, Date, lat, lon, station, id, dataset, family,genus) %>% 
  summarise(count=sum(count)) %>% 
  ungroup() %>% 
  group_by(taxa_raw, taxa_clean, lat, lon, station, id, dataset, family,genus) %>% 
  tidyr::complete(Date = seq(min(Date),max(Date), by="1 day"))%>%
  mutate(count_ma = roll_mean(count, n = 28, align = "right", fill = NA, na.rm = T)) %>%
  ungroup() %>% 
  drop_na(family) %>% 
  group_by(station, id, Date,family, dataset, lon, lat) %>% 
  summarise(count_agg=sum(count_ma)) %>% 
  ungroup() %>% 
  mutate(dataset="nab")

ggplot(data=df_comm_all%>% 
         filter(family=="Total") %>% 
         filter(id %in% (meta_df %>% slice(1:10) %>% pull(id))) ) +
  # geom_line(aes(x=Date, y=log(count_agg+1)))  +
  geom_line(aes(x=Date, y=count_agg))  +
  scale_y_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log", function(x) exp(x)),
                     labels = trans_format("log", math_format(e^.x)))+
  ggtitle("Total Spore Counts (all counts are per cubic meter of air)")+
  ylab("log (count)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2, scales = "free_y") 

df_comm<-df_comm_all%>% 
  filter(id %in% (meta_df %>% slice(1:10) %>% pull(id))) #%>% 
  # filter(Date>=as.Date("2009-01-01") & Date < as.Date("2021-01-01")) 

comm_meta<-df_comm %>% 
  group_by(family) %>% 
  summarize(median=median(count_agg, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(median))
df_comm_major<-df_comm %>% 
  filter(family %in% (comm_meta %>% slice(1:5) %>% pull(family))|
           family=="Total") 

ggplot(data=df_comm_major%>% filter(dataset=="nab")) +
  geom_line(aes(x=Date, y=count_agg, col=family, group=family)) +
  scale_y_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log", function(x) exp(x)),
                     labels = trans_format("log", math_format(e^.x)))+
  ggtitle("Taxa-specific Spore Counts (all counts are per cubic meter of air)")+
  ylab("log (count)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2, scales = "free_y")  

df_pheno<-df_comm_all %>%
  filter(family=="Cladosporiaceae") %>% 
  # filter(taxa== "Total Spore Count") %>% 
  # filter(id %in% (meta_df %>% slice(1:10) %>% pull(id))) %>% 
  mutate(year=as.integer(format(Date, "%Y"))) %>% 
  mutate(doy=as.integer(format(Date, "%j"))) %>% 
  group_by(station, id, lat, lon, year) %>% 
  mutate(cumsum=cumsum(coalesce(count_agg,0))) %>% 
  ungroup() %>% 
  filter(log(count_agg+1)>0) %>% 
  mutate(year=case_when(doy<=60 ~ (year-1),
                        TRUE ~ as.double(year))) %>% 
  mutate(doy=case_when(doy>60 ~ (doy-60),
                        TRUE ~ as.numeric(difftime( Date,as.Date(paste0(year,"-01-01")) )-60 )))

df_pheno_group<-df_pheno %>% 
  distinct(id, year, station, dataset, lat, lon) 
df_pheno_subset<-vector(mode="list",length=nrow(df_pheno_group))
for(i in 1:nrow(df_pheno_group)) {
  df_pheno_1y<-df_pheno %>% 
    filter(id==df_pheno_group$id[i],
           year==df_pheno_group$year[i])  %>% 
    complete(doy=seq(1,365,by=1)) %>% 
    mutate(station=df_pheno_group$station[i],
           id=df_pheno_group$id[i],
           lon=df_pheno_group$lon[i],
           lat=df_pheno_group$lat[i],
           dataset=df_pheno_group$dataset[i])
  if (nrow(df_pheno_1y %>% drop_na(count_agg))>=365*3/4) {
    df_pheno_subset[[i]]<-df_pheno_1y
  }
  print(i)
}
df_pheno_subset<-rlist::list.clean(df_pheno_subset) %>% 
  bind_rows()

ggplot(df_pheno_subset)+
  geom_line(aes(x=doy, y=log(count_agg+1), col=year, group=year), alpha=0.5)+
  # ylim(4,10)+
  scale_color_viridis_c()+
  theme_classic()+
  facet_wrap(.~id*station, ncol=4, scales = "free_y")

###
df_pheno_group<-df_pheno_subset %>% 
  distinct(id, year, station, dataset, lat, lon) 
for (i in 1:10) {
  df_pheno_1y<-df_pheno_subset %>% 
    right_join(df_pheno_group %>% sample_n(1))
  fit <- FitDoubleLogElmore(log(df_pheno_1y$count_agg+1),plot = T,hessian = T,ninit = 50)
}


p_load(strucchange)
p_load(Kendall)
p_load(bfast)
# can't install bcp
# R.home() and go in to find the path of R
# run R in vanilla mode
# https://stackoverflow.com/questions/64706243/microsoft-r-open-line-breaks-during-package-compilation
# /opt/microsoft/ropen/4.0.2/lib64/R/bin/R --vanilla
p_load(bcp)
p_load(phenopix)
# install.packages("./greenbrown_2.4.3.tar.gz", repos = NULL, type="source")
library(greenbrown)

df_pheno_group<-df_pheno %>% 
  distinct(id, year, station, dataset, lat, lon) 
param_list<-vector(mode="list",length=nrow(df_pheno_group))
for(i in 1:nrow(df_pheno_group)) {
  df_pheno_1y<-df_pheno %>% 
    filter(id==df_pheno_group$id[i],
           year==df_pheno_group$year[i])  %>% 
    complete(doy=seq(1,365,by=1))
  if (nrow(df_pheno_1y %>% drop_na(count_agg))<365*3/4) {
    param_list[[i]]<-data.frame(par=c("mn", "mx", "sos", "rsp", "eos", "rau"),
                                value=rep(NA, 6),
                                se=rep(NA, 6)) %>% 
      cbind(df_pheno_group[i,])
  } else {
    fit <- FitDoubleLogBeck(log(df_pheno_1y$count_agg+1),plot = T,hessian = T,ninit = 50)
    # plot(log(df_pheno_1y$count_ma+1))
    # lines(fit$predicted, col="blue")
    param_list[[i]]<-data.frame(par=names(fit$params),
                                value=fit$params[1:6], 
                                se=fit$stdError[1:6]) %>% 
      cbind(df_pheno_group[i,])
  }
  print(i)
}

param_df<-bind_rows(param_list) %>% 
  mutate(id=as.factor(id)) %>% 
  mutate(par=factor(par,levels=c("mn", "mx", "sos", "rsp", "eos", "rau"))) %>% 
  # filter(!par %in% c("rsp","rau")) %>% 
  remove_rownames() %>% 
  filter(se<=5)

ggplot(param_df)+
  geom_line(aes(x=year, y=value,group=id, col=id),alpha=0.5)+
  geom_errorbar(aes(x=year, ymin=value-1.95*se,ymax=value+1.95*se,group=id, col=id),alpha=0.5,width=0)+
  geom_smooth(aes(x=year, y=value,group=id, col=id),method=lm,se=F)+
  # geom_smooth(aes(x=year, y=value),method=lm,alpha=0.5,lwd=2)+
  facet_wrap(.~par, scales = "free_y")+
  theme_classic()
