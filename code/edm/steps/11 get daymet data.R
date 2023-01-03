daymet_df <- download_daymet(site = site,
                          lat = site_list$lat[soi],
                          lon = site_list$lon[soi],
                          start = max(pollen_df_subset %>%pull (date) %>% min() %>%  format("%Y") %>% as.integer(),1980),
                          end = min(pollen_df_subset %>%pull (date) %>% max() %>%  format("%Y") %>% as.integer(),2020),
                          internal = TRUE,
                          simplify = TRUE) %>% 
  mutate(date=as.Date(yday, origin = as.Date(paste0(year,"-01-01")))) %>% 
  spread(key="measurement", value="value") %>% 
  dplyr::select(date, tmax=`tmax..deg.c.`, tmin=`tmin..deg.c.`, prcp=`prcp..mm.day.`, vp=`vp..Pa.`) %>% 
  mutate(tmean=(tmax+ tmin)/2) %>% 
  dplyr::select(date, tmean, prcp, vp)

ts<-pollen_df_subset %>% 
  left_join(daymet_df, by="date") %>% 
  mutate(year=as.integer(format(date, "%Y")),
         site=1,
         pollen_sd=0.01) %>% 
  mutate(date=as.Date(date))

coord_df<-data.frame(lon=0, lat=0)
distMat<-matrix(0)
date_list<-unique(ts$date) %>% sort()

