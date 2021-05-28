rm(list=ls())
source("tail_analysis.R")
source("monthly_rarefy.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)
data_pt_thrs<-20
xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
grid_freshw<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/bt_freshw_min20yr_rawdata.RDS")
df<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")
df<-df%>%filter(site==431)
# these 3 sampling sites are 3 different lakes
#------------------------------------------------------
site<-df$site
x<-grid_freshw%>%filter(STUDY_ID==site)
x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
newsite<-sort(unique(x$newsite))
length(newsite)
# which newsite has atleast 20 yrs of sampling?
tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()
as.data.frame(table(tt$n))

latlon_esteban<-distinct(x,newsite,.keep_all=T)
latlon_esteban<-latlon_esteban%>%dplyr::select(LATITUDE,LONGITUDE)
write.csv(latlon_esteban,"./latlon_esteban.csv",row.names = F)

library(sf)
library(raster)
library(fasterize)
#setwd(dir="D:/BGB_Papers/BioDyn/DATA/for_BioTIME/raw_data/Hydrosheds/au_bas_30s_beta")
sp=st_read('../../DATA/for_BioTIME/raw_data/Hydrosheds/au_bas_30s_beta/au_bas_30s_beta.shp')
ras=raster(extent(sp),resolution=0.01) #creat an empty raster
ras2=fasterize(sp,ras,field='BASIN_ID',fun="first") #convert the polygons to raster, because polygons are overlapping, creating a problem to extract values then

latlon_esteban2<-st_as_sf(latlon_esteban,coords=c('LONGITUDE','LATITUDE'),crs=st_crs(sp)) #converting data to spatial data

latlon_esteban2$basin=extract(ras2,latlon_esteban2) #extrcating infromation
latlon_esteban=cbind(as.data.frame(latlon_esteban2),st_coordinates(latlon_esteban2))

ggplot()+
geom_point(data=latlon_esteban,aes(x=X,y=Y,color=as.factor(basin)))+
#geom_point(data=subset(latlon_esteban,basin==80124),aes(x=X,y=Y),col="red")
theme(legend.position="none")


