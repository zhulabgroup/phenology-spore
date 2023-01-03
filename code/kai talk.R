library(rgdal)
library(tidyverse)

fia_df<-read_csv("./RS4flower/data/species_back0.csv")
perc_df<-fia_df %>% 
  drop_na() %>% 
  filter(lat>=25 & lat<=50 & lon < -60 & lon> -130) %>% # focus on CONUS
  filter(dia>5) %>% # adult trees
  group_by(plt,lon,lat) %>% 
  # summarise(percent=sum(genus=="Betula")/n())
  summarise(percent=sum(genus=="Quercus")/n())

p_map<-
  ggplot() +
  geom_polygon( data=map_data("state"), aes(x=long, y=lat, group = group), fill="white" ) +
  # geom_point(data=perc_df[perc_df$percent==0,],aes(x=lon,y=lat),cex=0.5,color="white", alpha=0.5)+
  # geom_point(data=perc_df[perc_df$percent>0,],aes(x=lon,y=lat,color=percent),cex=0.5, alpha=0.5)+
  # scale_colour_gradient(low = "white", high = "#2D6EB0",
  #                       space = "Lab", na.value = "grey50", guide = "colourbar",
  #                       aesthetics = "colour",limits = c(0,1), breaks = c(0,0.5, 1))+
  geom_path( data=map_data("state"), aes(x=long, y=lat, group = group),color="grey50" ,alpha=0.5,lwd=0.2)+
  theme_void()+
  # theme(legend.position = c(0.05,0.05),
  #       legend.justification = c("left", "bottom"),
  #       legend.direction = "horizontal",
  #       legend.title  = element_blank())+
  # theme(legend.text=element_text(size=12))+
  # legend.title  = element_text(size = 10))
  # guides(color = guide_colourbar(barwidth = 5, barheight = 0.5))+
  guides(color = F)+
  coord_equal()
p_map


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

p_pollen_map<-p_map+
  geom_point(data=meta_df, aes(x=lon, y=lat), pch=10, color="black", cex=3)+ 
  coord_equal()
p_pollen_map

cairo_pdf("./nab/output/kai talk/map.pdf", height = 6, width = 12)
print(p_pollen_map)
dev.off()

#####
library(ptw)

pollen_df<-df_nab %>% 
  rename(taxa_raw=taxa) %>% 
  left_join(taxa_df, by="taxa_raw") %>% 
  mutate(family=case_when(taxa_raw=="Total Pollen Count"~"Total",
                          TRUE~family)) %>% 
  mutate(genus=case_when(taxa_raw=="Total Pollen Count"~"Total",
                         TRUE~genus)) %>% 
  filter(kingdom=="Viridiplantae"|is.na(kingdom)) %>% 
  left_join(meta_df %>% dplyr::select(id, site=location), by="id") %>% 
  group_by(Date, lat, lon, station, id, site, family,genus) %>% 
  summarise(count=sum(count) ) %>% 
  mutate(count= sqrt(count)) %>% 
  ungroup() %>% 
  group_by(lat, lon, station, id, site, family,genus) %>% 
  tidyr::complete(Date = seq(min(Date),max(Date), by="1 day"),fill=list(count=0)) %>% 
  mutate(count_ma = whit1(count ,30)) %>%
  # mutate(count_ma = roll_mean(count, n = 28, align = "center", fill = NA, na.rm = T)) %>%
  ungroup() %>% 
  mutate(date=as.Date(Date)) %>% 
  dplyr::select(-Date) %>% 
  mutate(year=format(date, "%Y") %>% as.numeric()) %>% 
  mutate(doy=format(date, "%j") %>% as.numeric()) %>% 
  drop_na(site)%>% 
  filter(genus=="Total")

p_pollen_ts<-ggplot(pollen_df  %>% mutate(year=as.factor(year)))+
  geom_line(aes(x=doy, y=count_ma , group=year, col=year))+
  facet_wrap(.~site, scales="free_y")+
  ylab("Pollen count per cubic meter ^ (1/2)") 

cairo_pdf("./nab/output/kai talk/time series.pdf", height = 6, width = 12)
print(p_pollen_ts)
dev.off()

# pollen_df_genus<-pollen_df %>% 
#   filter(genus=="Quercus")

library(daymetr)
library(pracma)
site_list<-pollen_df %>% pull(site) %>% unique() %>% sort()


pollen_climate_df_list<-vector(mode="list", length=length(site_list))
for (s in 1:length(site_list)) {
    
    df_subset<-pollen_df %>% 
      filter(site==site_list[s])
      
      meta<-df_subset %>% 
        slice(1) %>% 
        dplyr::select(lat, lon, site)
      df_daymet<- download_daymet(site = meta$site,
                                  lat = meta$lat,
                                  lon = meta$lon,
                                  start = min(df_subset$year),
                                  end = min(max(df_subset$year), 2020),
                                  internal = TRUE,
                                  simplify = TRUE) %>% 
        filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.","prcp..mm.day.")) %>% 
        spread(key = "measurement", value="value") %>% 
        rename(prcp=`prcp..mm.day.`,
               tmax=`tmax..deg.c.`,
               tmin=`tmin..deg.c.`) %>% 
        mutate(date=as.Date(yday, origin = paste0(year,"-01-01"))-1) %>% 
        mutate (temp=(tmax+tmin/2)) %>% 
        mutate(site=meta$site) %>%
        mutate(year=format(date, "%Y") %>% as.numeric()) %>% 
        dplyr::select(site, date, year,temp, prcp)
      
      df_daymet_annual<-df_daymet %>% 
        group_by(site,year) %>% 
        summarize(mat=mean(temp),
                  tap=sum(prcp)) %>% 
        ungroup()
      
      df_pollen_annual<-df_subset %>% 
        group_by(site,year) %>% 
        arrange(doy) %>% 
        summarize(n=n(),
                  mindoy=min(doy),
                  load=sum(count^2),
                  peak=findpeaks((count^2), sortstr = T)[1,2] %>% as.numeric()
                  ) %>% 
        mutate(peak=peak+mindoy-1) %>%
        # dplyr::select(-mindoy) %>% 
        ungroup()
      
      df_annual<-left_join(df_daymet_annual, df_pollen_annual,by=c("site", "year")) %>% 
        filter(n>300)
      
      pollen_climate_df_list[[s]]<-df_annual
  
}
pollen_climate_df<-bind_rows(pollen_climate_df_list)

p1<-ggplot(pollen_climate_df %>% mutate(year=as.factor(year)))+
  geom_point(aes(x=mat, y=peak, col=site))+
  geom_smooth(aes(x=mat, y=peak, group=site,col=site),method = "lm", se=F, lwd=0.5)+
  theme_classic()+
  xlab("Mean annual temperature (°C)")+
  ylab("Timing of peak pollen count (Julian day)")+
  guides(col=F)+
  ylim(0, 200)

p2<-ggplot(pollen_climate_df %>% mutate(year=as.factor(year)))+
  geom_point(aes(x=mat, y=load ^(1/2), col=site))+
  geom_smooth(aes(x=mat, y=load ^(1/2), group=site,col=site),method = "lm", se=F, lwd=0.5)+
  theme_classic()+
  xlab("Mean annual temperature (°C)")+
  ylab("Annual pollen integral per cubic meter of air ^(1/2)")+
  guides(col=F)

p3<-ggplot(pollen_climate_df %>% mutate(year=as.factor(year)))+
  geom_point(aes(x=tap, y=peak, col=site))+
  geom_smooth(aes(x=tap, y=peak, group=site,col=site),method = "lm", se=F, lwd=0.5)+
  theme_classic()+
  xlab("Total annual precipitation (mm)")+
  ylab("Timing of peak pollen count (Julian day)")+
  guides(col=F)+
  ylim(0, 200)

p4<-ggplot(pollen_climate_df %>% mutate(year=as.factor(year)))+
  geom_point(aes(x=tap, y=load ^(1/2), col=site))+
  geom_smooth(aes(x=tap, y=load ^(1/2), group=site,col=site),method = "lm", se=F, lwd=0.5)+
  theme_classic()+
  xlab("Total annual precipitation (mm)")+
  ylab("Annual pollen integral per cubic meter of air ^(1/2)")+
  guides(col=F)

library(gridExtra)

cairo_pdf("./nab/output/kai talk/with climate.pdf", height = 12, width = 12)
print(grid.arrange(p1, p2, p3, p4, nrow=2))
dev.off()
