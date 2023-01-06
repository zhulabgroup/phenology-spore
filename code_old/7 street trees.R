library(tidyverse)
library(RJSONIO)
library(foreach)
library(doSNOW)
cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

sjTreesIdsRaw<-RJSONIO::fromJSON("https://geo.sanjoseca.gov/server/rest/services/OPN/OPN_OpenDataService/MapServer/510/query?where=1%3D1&outFields=*&returnIdsOnly=true&outSR=4326&f=json")
sjTreesIds<-sjTreesIdsRaw$objectIds

sjTreesIds_groups<-split(sjTreesIds, ceiling(1:length(sjTreesIds)/200))

sjTrees_df_list<-foreach (g = 1:length(sjTreesIds_groups),
         # .combine = rbind,
         .packages = c("tidyverse")) %dopar%{
  sjTreesIds_group<-sjTreesIds_groups[[g]]
  sjTreesRaw<-RJSONIO::fromJSON(paste0("https://geo.sanjoseca.gov/server/rest/services/OPN/OPN_OpenDataService/MapServer/510/query?where=1%3D1&objectIds=",paste0(sjTreesIds_group,collapse =","),"&outFields=*&outSR=4326&f=json"))
  sjTrees<-sjTreesRaw$features

    sjTrees_df_group_list<-vector(mode="list", length=length(sjTrees))
  for (i in 1:length(sjTrees)) {
    sjTrees_df_group_list[[i]]<-bind_cols(sjTrees[[i]]$attributes %>% bind_cols()  ,
                                    sjTrees[[i]]$geometry %>% as.list()  %>% bind_cols() )
  }
  
  print(paste0(g, " out of ",length(sjTreesIds_groups)))
  bind_rows(sjTrees_df_group_list)
}
sjTrees_df<-bind_rows(sjTrees_df_list)
stopCluster(cl)

nrow(sjTrees_df)
write_csv(sjTrees_df, "nab/data/sj_street_trees.csv")

sjTrees_df<-read_csv("nab/data/sj_street_trees.csv",
                     col_types = list(NAME=col_character(),
                                      SPECIES=col_character(),
                                      GENUS=col_character(),
                                      TRUNKHEIGHT=col_double(),
                                      ADDRESSNUM=col_character(),
                                      TREEAGE=col_double()))%>% 
  rowwise() %>% 
  mutate(genus=str_split(NAMESCIENTIFIC,pattern = " ", simplify = T)[1]) %>% 
  ungroup() %>% 
  arrange(desc(ORIGINALINVENTORYDATE)) %>% 
  distinct(x, y, .keep_all = T) %>% 
  mutate(genus_id=as.integer(as.factor(genus))) 

ref_genus<-sjTrees_df %>% 
  group_by(genus, genus_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))
ref_genus

sjTrees_df_genus<-sjTrees_df %>% 
  filter(genus%in%c(taxaoi)) 

sjTrees_df_genus %>% 
  group_by(NAMESCIENTIFIC) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
AllCounty <- map_data("county") %>% 
  filter(region=="california") %>% 
  filter(subregion=="santa clara")

ggplot()+
  geom_polygon( data=AllCounty, aes(x=long, y=lat, group=group),
                  color="darkblue", fill="lightblue", size = .1 )+
  geom_point(data=sjTrees_df_genus,aes(x=x, y=y, col=genus),alpha=0.25)+
  geom_point(data=meta_df %>% filter(id==7),aes(x=lon, y=lat),cex=10,pch=10)+
  theme_classic()+
  coord_equal()

sjTrees_sp <- SpatialPointsDataFrame(coords = sjTrees_df_genus %>% dplyr::select(x, y),
                               data = sjTrees_df_genus %>% dplyr::select(genus_id),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

sjTrees_sp_reproj<-spTransform(sjTrees_sp, CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))

library(raster)
sjTrees_ras <- rasterFromXYZ(sjTrees_df %>%
                               dplyr::select(x, y,genus_id) %>% 
                               drop_na() %>% 
                               head(),
                             res=0.5)  



bbox_poly <- as(bbox, "SpatialPolygons")
sp::proj4string(bbox_poly) <- "+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0"

bbox_poly_reproj <- sp::spTransform(bbox_poly, CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"))
bbox_reproj<-extent(bbox_poly_reproj)

r <- raster(bbox_reproj,
            res=c(3,3), crs= "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")
#the data have about, 270000 cells. that is how the number of ncol and nrow specified. 

sjTrees_total <- rasterize(coordinates(sjTrees_sp_reproj), r,sjTrees_sp_reproj$genus_id, fun="count")
plot(sjTrees_ras)
hist(sjTrees_ras)

sjTrees_quercus<-sjTrees_ras==161
plot(sjTrees_quercus)
sjTrees_acer<-sjTrees_ras==4
plot(sjTrees_acer)
sjTrees_betula<-sjTrees_ras==20
plot(sjTrees_betula)
sjTrees_populus<-sjTrees_ras==153
plot(sjTrees_populus)

####
library(tidyverse)
library(RSQLite)


db_file <- "/raid/users/ysong67/GitHub/phenology/nab/data/urban.db"
con_urban <- dbConnect(SQLite(), db_file)
dbListTables(con_urban)

db_file <- "/data/ZHULAB/archives/FIA/FIADB/SQLite/FIADB-2020-08-24.db"
con_general <- dbConnect(SQLite(), db_file)
dbListTables(con_general)


tree_df <- dbGetQuery(con_urban, 
    "SELECT * FROM
    ID_TREE as tree
    "
                     ) %>%
  as_tibble()

tree_df %>%
  group_by(STATECD, UNITCD, COUNTYCD) %>% 
  summarise(n=n())

plot_df <- dbGetQuery(con_urban, 
                     "SELECT * FROM
    ID_PLOT as p
    "
) %>%
  as_tibble()

sp_df <- dbGetQuery(con_general, 
                      "SELECT * FROM
    REF_SPECIES as sp
    "
) %>%
  as_tibble()


fia_df<-tree_df %>% 
  filter(STATECD==6,
         UNITCD==6,
         COUNTYCD==73) %>% 
  dplyr::select( PLOTID, TREE, SPCD ) %>% 
  left_join(plot_df %>% 
              dplyr::select(PLOTID, LAT, LON),
            by="PLOTID") %>% 
  left_join(sp_df %>% 
              dplyr::select(SPCD, GENUS),
            by="SPCD")
  
ggplot(fia_df)+
  geom_point(aes(x=LON, y=LAT, col=GENUS))+
  theme_minimal()

fia_df_genus<-fia_df %>% 
  filter(GENUS%in%c("Quercus","Betula","Acer","Populus"))
