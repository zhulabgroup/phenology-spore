fit_df_list[[soi]]<-combine_df_fit %>% mutate(site=soi)
fore_df_list[[soi]]<-combine_df_fore %>% mutate(site=soi)
  
combine_df_fit_in<-combine_df_fit %>% filter(cat=="in-sample")%>% filter(complete.cases(pollen, value, climatology_mean))
combine_df_fit_out<-combine_df_fit %>% filter(cat=="out-of-sample")%>% filter(complete.cases(pollen, value, climatology_mean))

stats_edm<-bind_rows(data.frame(compare_stats( obs_ori=combine_df_fit_in$pollen, pred_ori=combine_df_fit_in$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="in-sample"),
                     data.frame(compare_stats( obs_ori=combine_df_fit_out$pollen, pred_ori=combine_df_fit_out$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site=site,
         group="edm") 

stats_climatology<-bind_rows(data.frame(compare_stats( obs_ori=combine_df_fit_in$pollen, pred_ori=combine_df_fit_in$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="in-sample"),
                             data.frame(compare_stats( obs_ori=combine_df_fit_out$pollen, pred_ori=combine_df_fit_out$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site=site,
         group="climatology") 
  
stats_fit_list[[soi]]<-bind_rows(stats_edm, stats_climatology) %>% 
  arrange(stats)

combine_df_fore_in<-combine_df_fore %>% filter(cat=="in-sample") %>% filter(complete.cases(pollen, value, climatology_mean))
combine_df_fore_out<-combine_df_fore %>% filter(cat=="out-of-sample")%>% filter(complete.cases(pollen, value, climatology_mean))

stats_edm<-bind_rows(data.frame(compare_stats( obs_ori=combine_df_fore_in$pollen, pred_ori=combine_df_fore_in$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="in-sample"),
                     data.frame(compare_stats( obs_ori=combine_df_fore_out$pollen, pred_ori=combine_df_fore_out$value,range=df_upper_lower[[1]]$range)) %>% 
                       mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site=site,
         group="edm") 

stats_climatology<-bind_rows(data.frame(compare_stats( obs_ori=combine_df_fore_in$pollen, pred_ori=combine_df_fore_in$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="in-sample"),
                             data.frame(compare_stats( obs_ori=combine_df_fore_out$pollen, pred_ori=combine_df_fore_out$climatology_mean,range=df_upper_lower[[1]]$range)) %>% 
                               mutate(cat="out-of-sample"))%>% 
  mutate(cat=factor(cat, levels=c("in-sample", "out-of-sample"))) %>% 
  gather(key="stats", value="value",-cat ) %>% 
  spread(key="cat", value="value") %>% 
  mutate(stats=factor(stats, levels=c("corr", "R2", "RMSE", "nRMSE"))) %>% 
  arrange(stats) %>% 
  mutate(site=site,
         group="climatology")

stats_fore_list[[soi]]<-bind_rows(stats_edm, stats_climatology) %>% 
  arrange(stats)

  
### Plot ###
colors <- c("observed" = "black",
            "edm" = "blue",
            "climatology" = "dark green")
  p1<-
    ggplot(combine_df_fit)+
    geom_line(aes(x=date, y=pollen, col="observed"),alpha=0.5)+
    geom_line(aes(x=date, y=value, col="edm"),alpha=0.5)+
    geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill="edm"),alpha=0.25)+
    theme_classic()+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors)+
    geom_vline(xintercept =middate, alpha=0.5)+
    guides(fill="none")+
    labs(x = "date",
         y = "count^(1/4)",
         color = "") +
    theme(legend.position="top") 
  p2<-
    ggplot(combine_df_fit)+
    geom_line(aes(x=date, y=pollen, col="observed"),alpha=0.5)+
    geom_line(aes(x=date, y=climatology_mean, col="climatology"),alpha=0.5)+
    # geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill="edm"),alpha=0.25)+
    theme_classic()+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors)+
    geom_vline(xintercept =middate, alpha=0.5)+
    guides(fill="none")+
    labs(x = "date",
         y = "count^(1/4)",
         color = "") +
    theme(legend.position="top") 
  
  cairo_pdf(paste0(path_sub, "analyses/fit_ts.pdf"))
  print(grid.arrange(p1, p2, ncol=1))
  dev.off()
  
  
  p<-ggplot(combine_df_fit %>% 
              filter(complete.cases(pollen, value, climatology_mean)) %>%
              dplyr::select(observed=pollen, edm=value, climatology=climatology_mean, cat) %>% 
              gather(key="model", value = "predicted", -observed, -cat))+
    stat_binhex(aes(x=observed, y=predicted),bins=100)+
    geom_abline(intercept=0, slope=1, col="red")+
    # xlab("observed")+
    # ylab("predicted")+
    theme_classic()+
    coord_equal()+
    scale_fill_viridis_c()+
    facet_wrap(.~cat*model, nrow=2)
  
  cairo_pdf(paste0(path_sub, "analyses/fit_corr.pdf"))
  print(p)
  dev.off()

  
  colors <- c("observed" = "black",
              "edm" = "blue",
              "climatology" = "dark green")
  p1<-
    ggplot(combine_df_fore )+
    geom_line(aes(x=date, y=pollen, col="observed"),alpha=0.5)+
    geom_line(aes(x=date, y=value, col="edm", group=start_date),alpha=0.5)+
    geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill="edm", group=start_date),alpha=0.25)+
    # geom_line(aes(x=date, y=climatology_mean, col="climatology"),alpha=0.5)+
    theme_classic()+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors)+
    geom_vline(xintercept =middate, alpha=0.5)+
    geom_vline(xintercept =date_list[fore_start_list], col="blue", alpha=0.25)+
    guides(fill="none")+
    labs(x = "date",
         y = "count^(1/4)",
         color = "") +
    theme(legend.position="top") 
  
  p2<-
    ggplot(combine_df_fore )+
    geom_line(aes(x=date, y=pollen, col="observed"),alpha=0.5)+
    # geom_line(aes(x=date, y=value, col="edm", group=start_date),alpha=0.5)+
    # geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill="edm", group=start_date),alpha=0.25)+
    geom_line(aes(x=date, y=climatology_mean, col="climatology"),alpha=0.5)+
    theme_classic()+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors)+
    geom_vline(xintercept =middate, alpha=0.5)+
    geom_vline(xintercept =date_list[fore_start_list], col="blue", alpha=0.25)+
    guides(fill="none")+
    labs(x = "date",
         y = "count^(1/4)",
         color = "") +
    theme(legend.position="top") 
  
  cairo_pdf(paste0(path_sub, "analyses/fore_ts.pdf"))
  print(grid.arrange(p1, p2, ncol=1))
  dev.off()

  p<-ggplot(combine_df_fore %>% 
              filter(complete.cases(pollen, value, climatology_mean)) %>%
              dplyr::select(observed=pollen, edm=value, climatology=climatology_mean, start_date, cat) %>% 
              gather(key="model", value = "predicted", -observed, -start_date, -cat))+
    stat_binhex(aes(x=observed, y=predicted),bins=100)+
    geom_abline(intercept=0, slope=1, col="red")+
    # xlab("observed")+
    # ylab("predicted")+
    theme_classic()+
    coord_equal()+
    scale_fill_viridis_c()+
    facet_wrap(.~cat*model, nrow=2)
  
  cairo_pdf(paste0(path_sub, "analyses/fore_corr.pdf"))
  print(p)
  dev.off()