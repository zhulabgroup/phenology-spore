source("./nab/code/edm/steps/01 utils.R")
source("./nab/code/edm/steps/02 settings.R")

cl <- makeCluster(num_part, outfile = "")
registerDoSNOW(cl)

path<-"./nab/archive/"
dir.create(path, recursive = T)


df_all<-read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds") %>% as_tibble()

meta_df<-df_all %>% 
  # filter(taxa=="Total Spore Count") %>% 
  filter(taxa=="Total Pollen Count") %>% 
  group_by(taxa, station, location, lat, lon, id) %>% 
  drop_na(count) %>% 
  filter(count>0) %>% 
  summarise(mindate=min(Date),
            maxdate=max(Date),
            n=n()) %>% 
  mutate(range=maxdate-mindate) %>%
  ungroup() %>% 
  arrange(desc(n))

taxa_df<-read_rds("/data/ZHULAB/phenology/nab/nab_dat_taxa.rds") %>% as_tibble()

taxa_list<-c("Cladosporiaceae")
# taxa_list<-c("Quercus")
for (taxaoi in taxa_list) {
  pollen_df_raw<-df_all %>% 
    rename(taxa_raw=taxa) %>% 
    left_join(taxa_df, by="taxa_raw") %>% 
    # mutate(family=case_when(taxa_raw=="Total Spore Count"~"Total",
    # TRUE~family)) %>% 
    # mutate(genus=case_when(taxa_raw=="Total Spore Count"~"Total",
    # TRUE~genus)) %>% 
    # filter(genus==taxaoi) %>% 
    filter(family==taxaoi) %>%
    group_by(taxa_raw, taxa_clean, Date, lat, lon, station, id) %>% 
    summarise(count=sum(count)) %>% 
    ungroup() %>% 
    group_by(taxa_raw, taxa_clean, lat, lon, station, id) %>% 
    tidyr::complete(Date = seq(min(Date),max(Date), by="1 day"))%>%
    mutate(count_ma = roll_mean(count, n = 28, align = "right", fill = NA, na.rm = T)) %>%
    ungroup() %>% 
    group_by(station, id, Date,lon, lat) %>% 
    summarise(count_agg=sum(count_ma)) %>% 
    ungroup()
  
  pollen_site_count<-pollen_df_raw %>% 
    mutate(year=as.integer(format(Date,"%Y"))) %>% 
    group_by(id) %>% 
    drop_na(count_agg) %>% 
    filter(count_agg>0 ) %>% 
    summarise(year_num=length(unique(year)),
              sample_num=n(),
              .groups="drop") %>% 
    arrange(desc(year_num, sample_num))
  # pollen_site_count
  
  pollen_df<-pollen_df_raw %>% 
    right_join(pollen_site_count, by="id") %>% 
    filter(year_num>5) %>%
    # filter(sample_num>5*365) %>%
    dplyr::select(site_id=id,
                  lon=lon,
                  lat=lat,
                  value=count_agg,
                  date=Date) %>% 
    mutate(value=(value)^(1/4))
  
  p<-ggplot(data=pollen_df  ) +
    geom_line(aes(x=date, y=value))  +
    ylab("(count)^(1/4)")+
    theme_classic()+
    facet_wrap(.~site_id, ncol=3, scales = "free_y") 
  dir.create(paste0(path,taxaoi), recursive = T)
  cairo_pdf(paste0(path,taxaoi, "/ts.pdf"))
  print(p)
  dev.off()
  
  
  
  site_list<-pollen_df %>% distinct(site_id, .keep_all=T) %>% 
    dplyr::select(-date, -value)
  
  fit_df_list<-fore_df_list<-stats_fit_list<-stats_fore_list<-vector(mode="list", length=nrow(site_list))
    for (soi in 1:nrow(site_list)) {
      site<-site_list$site_id[soi]
      
      pollen_df_subset<-pollen_df %>% 
        rename(pollen=value) %>% 
        filter(site_id==site)
      
      path_sub<-paste0(path,taxaoi, "/",site,"/")
      dir.create(path_sub, recursive = T)
      source("./nab/code/edm/steps/11 get daymet data.R")
      
      # use first half to train model
      middate=as.Date("2018-01-01")
      
      source("./nab/code/edm/steps/21 preprocess data.R")
      
      source("./nab/code/edm/steps/22 prepare embeddings.R")
      source("./nab/code/edm/steps/23 train GP model.R")
      
      # predict for whole duration
      source("./nab/code/edm/steps/24 fit.R")
      
      # multi-step forecast
      source("./nab/code/edm/steps/25 forecast.R")
      
      # output table and figure
      source("./nab/code/edm/steps/25 output table and figure.R")
    }
  
  fit_df<-bind_rows(fit_df_list)
  write_rds(fit_df,  paste0(path,taxaoi, "/fit.rds"))
  
  fore_df<-bind_rows(fore_df_list)
  write_rds(fore_df,  paste0(path,taxaoi, "/fore.rds"))
  
    stats_fit_df<-bind_rows(stats_fit_list) %>% 
      left_join(meta_df %>% dplyr::select(station, location, lat, lon, id), by=c("site"="id"))
    write_csv(stats_fit_df, paste0(path,taxaoi, "/stats_fit.csv"))
    
    stats_fore_df<-bind_rows(stats_fore_list) %>% 
      left_join(meta_df %>% dplyr::select(station, location, lat, lon, id), by=c("site"="id"))
    write_csv(stats_fore_df, paste0(path,taxaoi, "/stats_fore.csv"))
  
}

fit_df_in<-fit_df %>% filter(cat=="in-sample")%>% filter(complete.cases(pollen, value, climatology_mean))
fit_df_out<-fit_df %>% filter(cat=="out-of-sample")%>% filter(complete.cases(pollen, value, climatology_mean))

stats_edm<-bind_rows(data.frame(compare_stats( obs_ori=fit_df_in$pollen, pred_ori=fit_df_in$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="in-sample"),
                     data.frame(compare_stats( obs_ori=fit_df_out$pollen, pred_ori=fit_df_out$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site="all",
         group="edm") 

stats_climatology<-bind_rows(data.frame(compare_stats( obs_ori=fit_df_in$pollen, pred_ori=fit_df_in$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="in-sample"),
                             data.frame(compare_stats( obs_ori=fit_df_out$pollen, pred_ori=fit_df_out$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site="all",
         group="climatology") 

stats_fit_all<-bind_rows(stats_edm, stats_climatology) %>% 
  arrange(stats)

p<-ggplot()+
  geom_point(data=fit_df %>% 
               filter(complete.cases(pollen, value, climatology_mean)) %>%
               dplyr::select(observed=pollen, `GP-EDM`=value, climatology=climatology_mean, cat) %>% 
               gather(key="model", value = "predicted", -observed, -cat),
             aes(x=observed, y=predicted), alpha=0.02)+
  geom_text(data=stats_fit_all %>%
              filter(stats=="R2") %>%
              mutate(group=case_when(group=="edm"~"GP-EDM",
                     TRUE~group)) %>% 
              gather(key="cat", value="value", -site, -group, -stats) %>% 
              dplyr::select(cat, value, model=group)
              ,aes(5, 15, label=paste0("R^2 = ", round(value, 2))))+
  # stat_binhex(aes(x=observed, y=predicted),bins=100)+
  geom_abline(intercept=0, slope=1, col="red")+
  # xlab("observed")+
  # ylab("predicted")+
  theme_classic()+
  coord_equal()+
  scale_fill_viridis_c()+
  facet_wrap(.~cat*model, nrow=2)
cairo_pdf(paste0("./nab/output/figures/",taxaoi,"_fit_corr.pdf"))
print(p)
dev.off()



fore_df_in<-fore_df %>% filter(horizon<=14) %>%  filter(cat=="in-sample")%>% filter(complete.cases(pollen, value, climatology_mean))
fore_df_out<-fore_df %>% filter(horizon<=14) %>% filter(cat=="out-of-sample")%>% filter(complete.cases(pollen, value, climatology_mean))

stats_edm<-bind_rows(data.frame(compare_stats( obs_ori=fore_df_in$pollen, pred_ori=fore_df_in$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="in-sample"),
                     data.frame(compare_stats( obs_ori=fore_df_out$pollen, pred_ori=fore_df_out$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site="all",
         group="edm") 

stats_climatology<-bind_rows(data.frame(compare_stats( obs_ori=fore_df_in$pollen, pred_ori=fore_df_in$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="in-sample"),
                             data.frame(compare_stats( obs_ori=fore_df_out$pollen, pred_ori=fore_df_out$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site="all",
         group="climatology") 

stats_fore_all<-bind_rows(stats_edm, stats_climatology) %>% 
  arrange(stats)

p<-ggplot(fore_df %>% 
         filter(horizon<=14) %>% 
         filter(complete.cases(pollen, value, climatology_mean)) %>%
         dplyr::select(observed=pollen, `GP-EDM`=value, climatology=climatology_mean, cat) %>% 
         gather(key="model", value = "predicted", -observed, -cat))+
  geom_point(aes(x=observed, y=predicted), alpha=0.25)+
  geom_text(data=stats_fore_all %>%
              filter(stats=="R2") %>%
              mutate(group=case_when(group=="edm"~"GP-EDM",
                                     TRUE~group)) %>% 
              gather(key="cat", value="value", -site, -group, -stats) %>% 
              dplyr::select(cat, value, model=group)
            ,aes(3, 11, label=paste0("R^2 = ", round(value, 2))))+
  # stat_binhex(aes(x=observed, y=predicted),bins=100)+
  geom_abline(intercept=0, slope=1, col="red")+
  # xlab("observed")+
  # ylab("predicted")+
  theme_classic()+
  coord_equal()+
  scale_fill_viridis_c()+
  facet_wrap(.~cat*model, nrow=2)

cairo_pdf(paste0("./nab/output/figures/",taxaoi,"_fore_corr.pdf"))
print(p)
dev.off()

cairo_pdf(paste0("./nab/output/figures/",taxaoi,"_fore_corr_small.pdf"), height = 5, width = 5)
print(p)
dev.off()
