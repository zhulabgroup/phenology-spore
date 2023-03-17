data_smooth_df<-read_rds("~/spore_phenology/data/data_smooth.rds")

#peak concentration & date
peak_con<-data_smooth_df %>% 
  filter(!is.na(count_smooth)) %>% 
  group_by(location,id,year) %>% 
  summarise(peak_con=max(count_smooth))
parameters_df<-station_year_df %>% 
  distinct(id,.keep_all = TRUE) %>% 
  arrange(id) %>%
  dplyr::select(lat,lon,location,id) %>% 
  merge(peak_con,by="id") %>% 
  rename("location"="location.x") %>% 
  dplyr::select(lat,lon,location,id,year,peak_con) %>% 
  left_join(data_smooth_df,by=c("location"="location","id"="id","year"="year","peak_con"="count_smooth")) %>% 
  rename("peak_date"="date")

#integral in time window
integral_df<-data_smooth_df %>% 
  filter(!is.na(count_smooth)) %>% 
  filter(month %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct")) %>% 
  group_by(location,id,year) %>% 
  summarise(integral=sum(count_smooth)/n()*214)
parameters_df<-full_join(parameters_df,integral_df,by=c("location"="location","id"="id","year"="year"))

#growing season
#
threshold_df<-data_smooth_df %>% 
  filter(!is.na(count_smooth)) %>% 
  group_by(location,id,year) %>% 
  summarise(threshold_inte=0.3*sum(count_smooth))
#start_date & end_date
start_date<-c()
end_date<-c()
for (n in 1:194) {
  inte=0
  data<-data_smooth_df %>% 
    filter(id==threshold_df$id[n]) %>% 
    filter(year==threshold_df$year[n]) %>% 
    filter(!is.na(count_smooth)) %>% 
    arrange(count_smooth)
  for (i in 1:nrow(data)) {
    inte=inte+data$count_smooth[i]
    if (inte > threshold_df$threshold_inte[n]) {
      break
    }
  }
  threshold=data$count_smooth[i-1]
  data2<-data_smooth_df %>% 
    filter(id==threshold_df$id[n]) %>% 
    filter(year==threshold_df$year[n]) %>% 
    filter(!is.na(count_smooth)) %>% 
    filter(count_smooth>threshold) %>% 
    arrange(doy)
  start_date[n]=data2$date[1]
  end_date[n]=data2$date[nrow(data2)]
}
threshold_df$start<-as.Date(start_date)
threshold_df$end<-as.Date(end_date)
threshold_df<-threshold_df %>% 
  rename("start_date"="start") %>% 
  rename("end_date"="end") %>% 
  dplyr::select(-threshold_inte)
parameters_df<-full_join(parameters_df,threshold_df,by=c("location"="location","id"="id","year"="year"))

write_rds(parameters_df,"~/spore_phenology/data/parameters.rds")