library(tidyverse)
library(plotly)
library(zoo)
library(pacman)
p_load(RcppRoll)

station_df <- read_csv("/data/ZHULAB/phenology/nab/NAB stations.csv") %>%
  mutate(id = row_number())

us_map <- map_data("world") %>% filter(region == "USA")
ggplot()+
  geom_polygon(data = us_map %>% filter(lat > -150), aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_map(projection = "albers", parameters = c(20, 50)) +
  xlim(-130, -70)+
  ylim(20, 50)+
  geom_point(data=station_df,aes(x=lon, y=lat))+
  theme_classic()

df_all <- read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds")
date_list <- seq(min(df_all$Date), max(df_all$Date), by="1 day")
df_all <- df_all %>% 
  group_by(taxa, group,station, location, lat, lon, id) %>% 
  tidyr::complete(Date = date_list)%>%
  # mutate(count_inter = na.approx(count, na.rm=FALSE, maxgap = 7)) %>% 
  mutate(count_ma = roll_mean(count, n = 28, align = "center", fill = NA, na.rm = T)) %>%
  # mutate(count_loess = predict(loess(count_inter ~ Date, span = .5, data=.))) %>% 
  ungroup()

meta_df<-df_all %>% 
  filter(taxa=="Total Pollen Count"|taxa=="Total Spore Count") %>% 
  group_by(taxa, group,station, location, lat, lon, id) %>% 
  drop_na(count) %>% 
  filter(count>0) %>% 
  summarise(mindate=min(Date),
         maxdate=max(Date),
         n=n()) %>% 
  mutate(range=maxdate-mindate) %>%
  ungroup() %>% 
  arrange(group,desc(n))
  
# plot data
pollen_df<-df_all %>% 
  filter(group=="pollen") %>% 
  filter(taxa== "Total Pollen Count") %>% 
  filter(id %in% (meta_df %>% filter(group=="pollen") %>% head(10) %>% pull(id))) %>% 
  filter(Date>=as.Date("2015-01-01") & Date < as.Date("2018-01-01"))
pollen_gg <- ggplot(pollen_df) +
  geom_line(aes(Date, log(count+1))) +
  geom_line(aes(Date, log(count_ma+1)), col="red", alpha=0.75) +
  ggtitle("Pollen Counts (all counts are per cubic meter of air)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2)
# pollen_gg
ggplotly(pollen_gg)

spore_df<-df_all %>%
  filter(group=="spore") %>%
  filter(taxa== "Total Spore Count") %>% 
  filter(id %in% (meta_df %>% filter(group=="spore") %>% head(10) %>% pull(id))) %>% 
  filter(Date>=as.Date("2015-01-01") & Date < as.Date("2018-01-01"))
spore_gg <- ggplot(spore_df) +
  geom_line(aes(x=Date, y=log(count+1))) +
  geom_line(aes(Date, log(count_ma+1)), col="red", alpha=0.75) +
  ggtitle("Spore Counts (all counts are per cubic meter of air)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2)
# spore_gg
ggplotly(spore_gg)

pollen_comm<-df_all %>%
  filter(group=="pollen") %>%
  filter(taxa!= "Total Pollen Count") %>% 
  filter(id %in% (meta_df %>% filter(group=="pollen") %>% slice(1:4) %>% pull(id))) %>% 
  filter(Date>=as.Date("2015-01-01") & Date < as.Date("2016-01-01"))
ggplot(pollen_comm) +
  geom_line(aes(x=Date, y=log(count_ma+1), col=taxa, group=taxa), alpha=0.3) +
  ggtitle("Pollen Counts (all counts are per cubic meter of air)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2)+
  guides(col=F)
  # theme(legend.position="bottom")+
  # guides(col = guide_legend(label.position = "bottom"))

# cairo_pdf("./nab/figures/spore comm.pdf", height=12, width=12)
spore_comm<-df_all %>%
  filter(group=="spore") %>%
  filter(taxa!= "Total Spore Count") %>% 
  filter(id %in% (meta_df %>% filter(group=="spore") %>% slice(1:4) %>% pull(id))) %>% 
  filter(Date>=as.Date("2015-01-01") & Date < as.Date("2016-01-01"))
ggplot(spore_comm) +
  geom_line(aes(x=Date, y=log(count_ma+1), col=taxa, group=taxa), alpha=0.3) +
  ggtitle("Spore Counts (all counts are per cubic meter of air)")+
  theme_classic()+
  facet_wrap(.~station, ncol=2)+
  guides(col=F)
  # theme(legend.position="bottom")+
  # guides(col = guide_legend(label.position = "bottom"))
# dev.off()

spore_pheno<-df_all %>%
  filter(group=="spore") %>%
  filter(taxa== "Total Spore Count") %>% 
  filter(id %in% (meta_df %>% filter(group=="spore") %>% slice(1) %>% pull(id))) %>% 
  mutate(year=as.integer(format(Date, "%Y"))) %>% 
  mutate(doy=as.integer(format(Date, "%j"))) %>% 
  dplyr::select(-taxa, -group) %>% 
  filter(Date>=as.Date("2009-01-01") & Date < as.Date("2018-01-01")) %>% 
  group_by(year) %>% 
  mutate(cumsum=cumsum(coalesce(count_ma,0))) %>% 
  ungroup()

ggplot(spore_pheno)+
  geom_line(aes(x=doy, y=log(count_ma+1), col=year, group=year), alpha=0.5)+
  ylim(4,10)+
  scale_color_viridis_c()+
  theme_classic()#+
  # facet_wrap(.~year, ncol=1)

ggplot(spore_pheno)+
  geom_line(aes(x=doy, y=log(cumsum+1), col=year, group=year), alpha=0.5)+
  ylim(5,15)+
  scale_color_viridis_c()+
  theme_classic()#+

spore_doy<-df_all %>%
  filter(group=="spore") %>%
  filter(taxa== "Total Spore Count") %>% 
  mutate(year=as.integer(format(Date, "%Y"))) %>% 
  mutate(doy=as.integer(format(Date, "%j"))) %>% 
  drop_na() %>% 
  group_by(station, location, lat, lon, id,year) %>% 
  arrange(desc(count_ma)) %>% 
  slice(1)  %>% 
  ungroup() %>% 
  dplyr::select(-taxa, -group)
ggplot(spore_doy) +
  geom_point(aes(x=year, y=doy) )+
  geom_smooth(aes(x=year, y=doy),method = lm, col="blue") +
  ylim(0,365)+
  ggtitle("Timing of peak total spore count")+
  theme_classic()+
  facet_wrap(.~station, ncol=5)+
  guides(col=F)

spore_doy_reg<-spore_doy %>% 
  group_by(station, location, lat, lon, id) %>%
  do(broom::tidy(lm(doy ~ year, .))) %>%
  filter(term == "year") %>%
  dplyr::select(station, location, lat, lon, id, doy_roc = estimate, doy_se=std.error,doy_p = p.value)

ggplot(spore_doy_reg)+
  geom_point(aes(x=id, y=doy_roc))+
  geom_segment(aes(x=id, y=doy_roc-doy_se, xend=id, yend=doy_roc+doy_se))+
  geom_hline(aes(yintercept=0), lty=2)+
  theme_classic()
summary(spore_doy_reg$doy_roc, na.rm = T)

ggplot(spore_doy_reg)+
  geom_polygon(data = us_map %>% filter(lat > -150), aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_map(projection = "albers", parameters = c(20, 50)) +
  xlim(-130, -70)+
  ylim(20, 50)+
  geom_point(aes(x=lon, y=lat, col=doy_roc), cex=2)+
  scale_color_viridis_c()+
  theme_classic()

ggplot(spore_doy) +
  geom_point(aes(x=year, y=log(count_ma+1)) )+
  geom_smooth(aes(x=year, y=log(count_ma+1)),method = lm, col="blue") +
  ggtitle("Peak total spore count")+
  theme_classic()+
  facet_wrap(.~station, ncol=5)+
  guides(col=F)

spore_pc_reg<-spore_doy %>% 
  group_by(station, location, lat, lon, id) %>%
  do(broom::tidy(lm(log(count_ma+1) ~ year, .))) %>%
  filter(term == "year") %>%
  dplyr::select(station, location, lat, lon, id, pc_roc = estimate, pc_se=std.error,pc_p = p.value)

ggplot(spore_pc_reg)+
  geom_point(aes(x=id, y=pc_roc))+
  geom_segment(aes(x=id, y=pc_roc-pc_se, xend=id, yend=pc_roc+pc_se))+
  geom_hline(aes(yintercept=0), lty=2)+
  theme_classic()
summary(spore_pc_reg$pc_roc, na.rm = T)

ggplot(spore_pc_reg)+
  geom_polygon(data = us_map %>% filter(lat > -150), aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_map(projection = "albers", parameters = c(20, 50)) +
  xlim(-130, -70)+
  ylim(20, 50)+
  geom_point(aes(x=lon, y=lat, col=pc_roc), cex=2)+
  scale_color_viridis_c()+
  theme_classic()

# Claudia for changepoint analysis
# Robert R in Statistics

# threshold? read medicine and health
# taxsize, taxonstand
