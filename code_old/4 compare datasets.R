library(tidyverse)

taxa_df<-read_csv("./nab/nab_and_inat_taxa.csv")
df_nab<- read_rds("/data/ZHULAB/phenology/nab/nab_dat_YS.rds")

meta_df<-df_nab %>% 
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


library(sf)
coord_nab<-df_nab %>% 
  rename(lat_nab=lat,
         lon_nab=lon) %>% 
  distinct(lat_nab, lon_nab) %>% 
  st_as_sf(coords = c("lon_nab","lat_nab"), remove = F)
coord_inat<-flower %>% 
  rename(lat_inat=latitude,
         lon_inat=longitude) %>% 
  distinct(lon_inat,  lat_inat) %>% 
  st_as_sf(coords = c("lon_inat","lat_inat"), remove = F)
nab_buffer = st_buffer(coord_nab, 1)

coord_inter<-vector(mode="list", length=nrow(coord_nab))
for (i in 1:nrow(coord_nab)) {
  coord_inter[[i]] = st_intersection(nab_buffer[i,], coord_inat) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    left_join(meta_df %>% dplyr::select(station, lat, lon, id), by=c("lat_nab"="lat", "lon_nab"="lon"))
  print(i)
}
coord_inter<-bind_rows(coord_inter)

coord_nab_inter<-coord_inter%>% 
  drop_na(id) %>% 
  distinct(lat_nab, lon_nab,.keep_all = T) %>% 
  st_as_sf(coords = c("lon_nab","lat_nab"), remove = F)  
coord_inat_inter<-coord_inter %>% 
  drop_na(id) %>% 
  distinct(lon_inat,  lat_inat,.keep_all = T) %>% 
  st_as_sf(coords = c("lon_inat","lat_inat"), remove = F)
nab_buffer_inter = st_buffer(coord_nab_inter, 1)
plot(nab_buffer_inter["id"],col=NA,border=nab_buffer_inter$id)
plot(coord_inat_inter["id"], col=coord_inat_inter$id,add=T)

df_combine<-vector(mode="list", length=nrow(meta_df))
for (i in 1:nrow(meta_df)) {
  df_nab_subset<-df_nab %>% 
    as_tibble() %>% 
    dplyr::select(-group, -location) %>% 
    filter(id==meta_df$id[i]) %>% 
    mutate(Date=as.Date(Date)) %>% 
    rename(taxa_raw=taxa)%>% 
    left_join(taxa_df, by=c("taxa_raw")) %>%
    filter(kingdom=="Viridiplantae") %>%
    group_by(taxa_clean, Date, lat, lon, station, id, taxa_raw, dataset, family,genus) %>% 
    summarize(count=sum(count)) %>% 
    ungroup() %>% 
    group_by(taxa_clean, lat, lon, station, id, taxa_raw, dataset, family, genus) %>% 
    complete(Date = seq(min(Date),max(Date), by="1 day"))%>%
    mutate(count_ma = roll_mean(count, n = 28, align = "center", fill = NA, na.rm = T)) %>%
    ungroup()
  
  df_inat_subset<-flower %>% 
    right_join(coord_inter %>% filter(id==meta_df$id[i]) ,
              by=c("longitude"="lon_inat",
                   "latitude"="lat_inat")) %>% 
    dplyr::select(-longitude, -latitude, -id_iNat, -flowers) %>% 
    rename(taxa_clean=scientific_name, Date=observed_on, lon=lon_nab, lat=lat_nab) %>% 
    left_join(taxa_df, by="taxa_clean") %>% 
    group_by(taxa_clean, Date, lat, lon, station, id, taxa_raw, dataset, family,genus) %>% 
    summarize(count=n()) %>% 
    ungroup() %>% 
    group_by(taxa_clean, lat, lon, station, id, taxa_raw, dataset, family, genus) %>% 
    complete(Date = seq(min(Date),max(Date), by="1 day"))%>%
    mutate(count_ma = roll_mean(count, n = 28, align = "center", fill = NA, na.rm = T)) %>%
    ungroup()
  df_combine[[i]]<-bind_rows(df_nab_subset, df_inat_subset)
}
df_combine<-bind_rows(df_combine)

common_family<-intersect(taxa_df %>% filter(dataset=="nab") %>% pull(family) %>% unique() ,
          taxa_df %>% filter(dataset=="inat") %>% pull(family) %>% unique())
df_comm<-df_combine %>%
  # filter(taxa_raw!= "Total Pollen Count") %>% 
  filter(id %in% (meta_df %>% slice(1:4) %>% pull(id))) %>% 
  filter(Date>=as.Date("2009-01-01") & Date < as.Date("2021-01-01")) %>%
  group_by(station, id, Date,family, dataset) %>% 
  summarize(count_agg=sum(count_ma)) %>% 
  ungroup() %>% 
  drop_na(family)

ggplot() +
  geom_line(data=df_comm%>% filter(dataset=="nab"),aes(x=Date, y=log(count_agg+1), col=family, group=family)) +
  ggtitle("Taxa-specific Pollen Counts (all counts are per cubic meter of air)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2, scales = "free_y")

ggplot() +
  geom_line(data=df_comm %>% filter(dataset=="inat"),aes(x=Date, y=log(count_agg+1), col=family, group=family), lwd=1) +
  ggtitle("Taxa-specific Flowering Observations (sum of count)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2, scales = "free_y")


ggplot() +
  geom_line(data=df_comm %>% filter(family %in% common_family)%>% filter(dataset=="nab"),aes(x=Date, y=log(count_agg+1), col=family, group=family)) +
  geom_line(data=df_comm %>% filter(family %in% common_family)%>% filter(dataset=="inat"),aes(x=Date, y=log(count_agg+1), col=family, group=family), lwd=2) +
  ggtitle("Taxa-specific Flowering Phenology")+
  theme_classic()+
  facet_wrap(.~family*station, ncol=2, scales = "free_y")

