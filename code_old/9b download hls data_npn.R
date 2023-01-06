npn_data<-read_csv("/data/ZHULAB/phenology/NPN/status_intensity_observation_data.csv")
View(npn_data)

npn_data_proc<-npn_data %>% 
  filter(Phenophase_Status>=0) %>% 
  group_by(Site_ID,Observation_Date ) %>% 
  summarise(proportion=sum(Phenophase_Status==1)/n()) %>% 
  ungroup()

ggplot(npn_data_proc)+
  geom_point(aes(Observation_Date, proportion))+
  geom_line(aes(Observation_Date, proportion))+
  facet_wrap(.~Site_ID)+
  theme_classic()

# site 35875

site_data<-read_csv("/data/ZHULAB/phenology/NPN/ancillary_site_data.csv")
site_data %>% 
  filter(Site_ID==35875) %>% 
  View()

npn_data %>% 
  filter(Site_ID==35875)

# download HLS L30 data
site<-"HARV"
site<-"SJ"

path_hls<-paste0("/data/ZHULAB/phenology/HLS_L30/", site, "/")

if (site=="HARV") {
  hls_sp<-SpatialPoints(data.frame(lon=-72.17213,lat= 42.54278))
}
if (site=="SJ") {
  hls_sp<-SpatialPoints(data.frame(lon=-121.90860011992854,lat=37.28380167967409))
}
crs(hls_sp)<-"+proj=longlat +datum=WGS84"
hls_sp_reproj<-spTransform(hls_sp, CRS("+proj=utm +zone=18 +ellps=WGS84 +units=m +no_defs "))

coord_df<-coordinates(hls_sp_reproj) %>% 
  as_tibble() %>% 
  mutate(id=row_number())

cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

# Fmask
files<-list.files(path_hls, "*Fmask.tif", recursive = T, full.names = T)
time_df<-list.files(path_hls, "*Fmask.tif", recursive = T) %>% 
  str_split(pattern="\\.", simplify = T) %>% 
  data.frame() %>% 
  dplyr::select(filename=X4) %>% 
  mutate(year=substr(filename,1,4)%>% as.integer(),
         doy=substr(filename,5,7) %>% as.integer()) %>% 
  mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
  mutate(f=row_number()) %>% 
  dplyr::select(-filename)

nday<-length(files)
nloc<-length(hls_sp_reproj)
Fmask_mat<-foreach (f = 1:nday,
                   .packages = c("raster"),
                   .combine="rbind") %dopar%{
                     file<-files[f]
                     hls_ras<-raster(file)
                     
                     hls_values<-cbind(Fmask=raster::extract(hls_ras,hls_sp_reproj) ,f, id=1:nloc)
                     print(paste0( f, " out of ", nday))
                     hls_values[complete.cases(hls_values),]
                   }

Fmask_df<-Fmask_mat %>% 
  as_tibble() %>% 
  left_join(time_df, by="f") %>% 
  left_join(coord_df, by="id") %>% 
  dplyr::select(-f)


# blue
files<-list.files(path_hls, "*B02.tif", recursive = T, full.names = T)
time_df<-list.files(path_hls, "*B02.tif", recursive = T) %>% 
  str_split(pattern="\\.", simplify = T) %>% 
  data.frame() %>% 
  dplyr::select(filename=X4) %>% 
  mutate(year=substr(filename,1,4)%>% as.integer(),
         doy=substr(filename,5,7) %>% as.integer()) %>% 
  mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
  mutate(f=row_number()) %>% 
  dplyr::select(-filename)

nday<-length(files)
nloc<-length(hls_sp_reproj)
blue_mat<-foreach (f = 1:nday,
                 .packages = c("raster"),
                 .combine="rbind") %dopar%{
                   file<-files[f]
                   hls_ras<-raster(file)
                   
                   hls_values<-cbind(blue=raster::extract(hls_ras,hls_sp_reproj) ,f, id=1:nloc)
                   print(paste0( f, " out of ", nday))
                   hls_values[complete.cases(hls_values),]
                 }

blue_df<-blue_mat %>% 
  as_tibble() %>% 
  left_join(time_df, by="f") %>% 
  left_join(coord_df, by="id") %>% 
  dplyr::select(-f)

# green
files<-list.files(path_hls, "*B03.tif", recursive = T, full.names = T)
time_df<-list.files(path_hls, "*B03.tif", recursive = T) %>% 
  str_split(pattern="\\.", simplify = T) %>% 
  data.frame() %>% 
  dplyr::select(filename=X4) %>% 
  mutate(year=substr(filename,1,4)%>% as.integer(),
         doy=substr(filename,5,7) %>% as.integer()) %>% 
  mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
  mutate(f=row_number()) %>% 
  dplyr::select(-filename)

nday<-length(files)
nloc<-length(hls_sp_reproj)
green_mat<-foreach (f = 1:nday,
                   .packages = c("raster"),
                   .combine="rbind") %dopar%{
                     file<-files[f]
                     hls_ras<-raster(file)
                     
                     hls_values<-cbind(green=raster::extract(hls_ras,hls_sp_reproj) ,f, id=1:nloc)
                     print(paste0( f, " out of ", nday))
                     hls_values[complete.cases(hls_values),]
                   }

green_df<-green_mat %>% 
  as_tibble() %>% 
  left_join(time_df, by="f") %>% 
  left_join(coord_df, by="id") %>% 
  dplyr::select(-f)

# red
files<-list.files(path_hls, "*B04.tif", recursive = T, full.names = T)
time_df<-list.files(path_hls, "*B04.tif", recursive = T) %>% 
  str_split(pattern="\\.", simplify = T) %>% 
  data.frame() %>% 
  dplyr::select(filename=X4) %>% 
  mutate(year=substr(filename,1,4)%>% as.integer(),
         doy=substr(filename,5,7) %>% as.integer()) %>% 
  mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
  mutate(f=row_number()) %>% 
  dplyr::select(-filename)

nday<-length(files)
nloc<-length(hls_sp_reproj)
red_mat<-foreach (f = 1:nday,
                   .packages = c("raster"),
                   .combine="rbind") %dopar%{
                     file<-files[f]
                     hls_ras<-raster(file)
                     
                     hls_values<-cbind(red=raster::extract(hls_ras,hls_sp_reproj) ,f, id=1:nloc)
                     print(paste0( f, " out of ", nday))
                     hls_values[complete.cases(hls_values),]
                   }

red_df<-red_mat %>% 
  as_tibble() %>% 
  left_join(time_df, by="f") %>% 
  left_join(coord_df, by="id") %>% 
  dplyr::select(-f)

# nir
files<-list.files(path_hls, "*B05.tif", recursive = T, full.names = T)
time_df<-list.files(path_hls, "*B05.tif", recursive = T) %>% 
  str_split(pattern="\\.", simplify = T) %>% 
  data.frame() %>% 
  dplyr::select(filename=X4) %>% 
  mutate(year=substr(filename,1,4)%>% as.integer(),
         doy=substr(filename,5,7) %>% as.integer()) %>% 
  mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
  mutate(f=row_number()) %>% 
  dplyr::select(-filename)

nday<-length(files)
nloc<-length(hls_sp_reproj)
nir_mat<-foreach (f = 1:nday,
                   .packages = c("raster"),
                   .combine="rbind") %dopar%{
                     file<-files[f]
                     hls_ras<-raster(file)
                     
                     hls_values<-cbind(nir=raster::extract(hls_ras,hls_sp_reproj) ,f, id=1:nloc)
                     print(paste0( f, " out of ", nday))
                     hls_values[complete.cases(hls_values),]
                   }

nir_df<-nir_mat %>% 
  as_tibble() %>% 
  left_join(time_df, by="f") %>% 
  left_join(coord_df, by="id") %>% 
  dplyr::select(-f)

# stopCluster(cl)


hls_df<-Fmask_df %>% 
  full_join(blue_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
  full_join(green_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
  full_join(red_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
  full_join(nir_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
  mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1),
         gndvi=(nir-green)/(nir+green),
         ebi= ((red+green+blue))/ (green/blue * (red - blue + 1))) %>% 
  mutate(rgb=(red+green+blue)) %>%
  mutate(red=red/rgb,
         green=green/rgb,
         blue=blue/rgb) %>%
  mutate(qa=case_when(Fmask==0|Fmask==64 ~2,
                      TRUE~1))

hls_prop_df<-full_join(hls_df,
                       npn_data_proc %>% 
                         filter(Site_ID==35875) %>% 
                         dplyr::select(date=Observation_Date, proportion) %>% 
                         mutate(id=1)
                         # mutate(year=format(date, "%Y") %>%
                         #          as.numeric()) %>%
                         # filter(year%in% 2018:2021) %>% 
                         # dplyr::select(-year)
                         , by=c("date", "id") ) %>% 
  arrange(date)

p<-ggplot(data=hls_prop_df %>% 
            dplyr::select(-Fmask) %>% 
            filter(qa==2|is.na(qa)) %>% 
            filter((evi>0& evi<1)|is.na(evi)) %>% 
            # filter(ebi>0, ebi<5) %>%
            gather(key="var", value="value", -date, -id, -doy, -year, -lon, -lat, -qa) #%>% 
            # filter(!var %in% c("gndvi", "red", "green", "blue", "nir"))
          )+
  geom_point(aes(date, value,group=id, col=var,alpha=0.5*qa))+
  # geom_smooth(aes(date, value,group=id, col=var),alpha=0.1, span=0.3)+
  # geom_quantile(aes(date, value),method = "rqss", lambda = 10, quantiles=c( .05 , .5 , .95 ), col="black")+
  theme_classic()+
  geom_vline(xintercept = as.Date(paste0(2018:2021, "-01-01"))) +
  facet_wrap(.~var, ncol=1, scales = "free_y") 
  
p
