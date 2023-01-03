library(RcppRoll)
taxa_df<-read_csv("./nab/data/nab_and_inat_taxa.csv")
df_nab<- read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds")

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

###
df_comm_all<-df_nab %>% 
  rename(taxa_raw=taxa) %>% 
  left_join(taxa_df, by="taxa_raw") %>% 
  mutate(family=case_when(taxa_raw=="Total Pollen Count"~"Total",
                          TRUE~family)) %>% 
  mutate(genus=case_when(taxa_raw=="Total Pollen Count"~"Total",
                         TRUE~genus)) %>% 
  filter(kingdom=="Viridiplantae"|is.na(kingdom)) %>% 
  group_by(taxa_raw, taxa_clean, Date, lat, lon, station, id, dataset, family,genus) %>% 
  summarise(count=sum(count)) %>% 
  ungroup() %>% 
  group_by(taxa_raw, taxa_clean, lat, lon, station, id, dataset, family,genus) %>% 
  tidyr::complete(Date = seq(min(Date),max(Date), by="1 day")) %>% 
  mutate(count_ma = roll_mean(count, n = 28, align = "center", fill = NA, na.rm = T)) %>%
  ungroup() %>% 
  mutate(Date=as.Date(Date))

df_comm_sj<-df_comm_all %>% 
  filter(id==7) %>% 
  filter(genus%in%c("Quercus","Betula","Acer","Populus"))

p1<-ggplot(df_comm_sj) +
  geom_line(aes(Date, (count)^(1/4), col=genus)) +
  geom_line(aes(Date, (count_ma)^(1/4)), col="red") +
  ggtitle("San Jose")+
  theme_classic()+
  geom_vline(xintercept = as.Date(paste0(2017:2021, "-01-01")))+
  facet_wrap(.~genus, ncol=1)


df_comm_sd<-df_comm_all %>% 
  filter(id==23) %>% 
  filter(genus%in%c("Quercus","Betula","Acer","Populus"))

p2<-ggplot(df_comm_sd) +
  geom_line(aes(Date, (count)^(1/4), col=genus)) +
  geom_line(aes(Date, (count_ma)^(1/4)), col="red") +
  ggtitle("San Diego")+
  theme_classic()+
  geom_vline(xintercept = as.Date(paste0(2017:2021, "-01-01")))+
  facet_wrap(.~genus, ncol=1)

grid.arrange(p1, p2, ncol=2)
