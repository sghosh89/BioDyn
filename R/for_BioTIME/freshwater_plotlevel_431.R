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

site<-df$site
x<-grid_freshw%>%filter(STUDY_ID==site)
x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
newsite<-sort(unique(x$newsite))
length(newsite)
# which newsite has atleast 20 yrs of sampling?
tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()
as.data.frame(table(tt$n))

# we need to group the newsite based on hydrobasins
latlon_esteban<-distinct(x,newsite,.keep_all=T)
latlon_esteban<-latlon_esteban%>%dplyr::select(LATITUDE,LONGITUDE)
#write.csv(latlon_esteban,"./latlon_esteban.csv",row.names = F)

library(sf)
library(raster)
library(fasterize)

sp<-st_read('../../DATA/for_BioTIME/raw_data/Hydrosheds/au_bas_30s_beta/au_bas_30s_beta.shp')
ras<-raster(extent(sp),resolution=0.01) #creat an empty raster
ras2<-fasterize(sp,ras,field='BASIN_ID',fun="first") #convert the polygons to raster, because polygons are overlapping, creating a problem to extract values then

latlon_esteban2<-st_as_sf(latlon_esteban,coords=c('LONGITUDE','LATITUDE'),crs=st_crs(sp)) #converting data to spatial data

latlon_esteban2$basin<-extract(ras2,latlon_esteban2) #extrcating infromation
latlon_esteban<-cbind(as.data.frame(latlon_esteban2),st_coordinates(latlon_esteban2))
latlon_esteban<-rename(latlon_esteban,LATITUDE=Y,LONGITUDE=X)

ggplot()+
  geom_point(data=latlon_esteban,aes(x=LONGITUDE,y=LATITUDE,color=as.factor(basin)))+
  geom_point(data=subset(latlon_esteban,basin==80355),aes(x=LONGITUDE,y=LATITUDE),col="black")+
  theme(legend.position="none")

latlon_esteban<-latlon_esteban%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
latlon_esteban<-latlon_esteban%>%dplyr::select(newsite,basin)

x<-left_join(x,latlon_esteban,by="newsite")
length(unique(x$basin))

# now we are going to aggregate by basin
x_agg<-x%>%group_by(basin,YEAR,MONTH,Species)%>%summarise(Abundance=mean(Abundance),
                                                              Biomass=mean(Biomass))%>%ungroup()

tt<-x%>%group_by(basin)%>%summarise(n=n_distinct(YEAR))%>%ungroup()

# include sites which are sampled > 20 years
tt<-tt%>%filter(n>=20)

# only one basin 80355 sampled for 27 years
x_agg<-x_agg%>%filter(basin==80355)
x_agg$CLIMATE<-unique(x$CLIMATE)
x_agg$REALM<-unique(x$REALM)
x_agg$TAXA<-unique(x$TAXA)
x_agg$ABUNDANCE_TYPE<-unique(x$ABUNDANCE_TYPE)
x_agg$BIOMASS_TYPE<-unique(x$BIOMASS_TYPE)
x_agg$site<-unique(x$STUDY_ID)
x_agg<-rename(x_agg,newsite=basin)
x_agg$newsite<-paste("STUDY_ID_431_basin_",x_agg$newsite,sep="")

site<-unique(x_agg$site)
#----------- create result folder for wrangle data -------------------------
resloc<-"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/431/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
newsite<-unique(x_agg$newsite)
saveRDS(newsite,paste(resloc,"newsite.RDS",sep=""))

# Now, create folder for all these newsite
for(k in 1:length(newsite)){
  resloc2<-paste("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/431/",newsite[k],"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
}

#------------ now format the data as per input format for tail analysis ------------

for(k in 1:length(newsite)){
  id<-which(x_agg$newsite%in%newsite[k])
  x<-x_agg[id,]
  
  # do not consider these unknown sp into analysis
  x<-x%>%filter(Species%notin%c("unspecifiable ","Unknown","Unknown rotifer", "Unknown rotifer2", "unknown ","Unknown "))
  
  t0<-x%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH))%>%ungroup()
  #t1<-x%>%group_by(YEAR,MONTH)%>%summarise(nd=n_distinct(DAY))%>%ungroup()
  
  #---------- ok, after seeing t0, we need to rarefy --------------
  min_samp<-min(t0$nm) # min months sampled each year
  cat("---------- min_samp = ",min_samp," , newsite = ",newsite[k]," ------------------- \n")
  need_rarefy<-length(unique(t0$nm))>1
  
  AB<-is.na(x$ABUNDANCE_TYPE)[1]
  if(AB==F){
    field<-"Abundance"
  }else{
    field<-"Biomass"
  }
  
  id<-which(colnames(x)==field)
  
  if(need_rarefy==T){
    study<-x%>%dplyr::select(MONTH,YEAR,Species,Value=id)
    x_c<-monthly_rarefy(study = study,resamples = 100,field = field)
  }else{
    x<-x%>%select(YEAR,Species,Value=id)
    x<-x%>%group_by(Species,YEAR)%>%
      dplyr::summarise(Value=mean(Value))%>%ungroup()
    c1<-x%>%group_by(Species)%>%summarise(n_distinct(YEAR))%>%ungroup() 
    # As all species are not found each year, we need to fill in the missing values with 0.
    x_c<-x %>% 
      complete(Species, 
               nesting(YEAR), 
               fill = list(Value = 0))
  }
  
  #-----------------------------------------------------------------
  xmat<-x_c%>%spread(Species, Value)
  year<-xmat$YEAR
  xmat<-as.matrix(xmat[,-1])
  rownames(xmat)<-year
  
  xmeta<-xxm%>%filter(STUDY_ID==site)
  
  input_sp<-list(spmat=xmat,meta=xmeta)
  resloc<-paste("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/431/",newsite[k],"/",sep="")
  saveRDS(input_sp,paste(resloc,"spmat.RDS",sep=""))
  
  #----------- saving input spmat for tailanal ---------------------
  m<-readRDS(paste(resloc,"spmat.RDS",sep=""))
  # first we aggregated the rare sp (present even less than 30% of sampled years) into a pseudo sp 
  presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
  presentyr<-unname(presentyr)
  commonspid<-which(presentyr>=0.7*nrow(m$spmat)) # consider species with >70% present yr
  rareid<-which(presentyr<0.7*nrow(m$spmat)) 
  ncol(m$spmat)==length(rareid) # that means not all sp are rare
  
  if(length(rareid)!=0){
    raresp<-m$spmat[,rareid]
    raresp<-as.matrix(raresp) # this line is for when you have only one rare sp
    raresp<-apply(X=raresp,MARGIN=1,FUN=sum)
    m1<-m$spmat[,commonspid]
    m1<-cbind(m1,raresp=raresp)
    m1<-as.data.frame(m1)
    input_tailanal<-m1
  }else{
    m1<-m$spmat
    input_tailanal<-m1
  }
  saveRDS(input_tailanal,paste(resloc,"input_tailanal.RDS",sep=""))
  
  #----------------- now do tail analysis ----------------------
  resloc2<-paste("../../Results/for_BioTIME/Freshwater_plotlevel/",site,"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
  
  #----------- analysis with covary sp ----------------
  resloc<-paste(resloc2,newsite[k],"/",sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  
  nsp<-ncol(input_tailanal)# including raresp column as last column
  if(nsp>2){
    res<-tail_analysis(mat = input_tailanal, resloc = resloc, nbin = 2)
  }
}




