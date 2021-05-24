rm(list=ls())
library(tidyverse)
`%notin%` <- Negate(`%in%`)
bt<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_query_data_metadata.RDS")
data_pt_thrs<-20 # 20 years minimum

grid <- bt %>% 
  dplyr::select(STUDY_ID, PLOT, DAY, MONTH, YEAR, 
                GENUS_SPECIES, sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS,
                CLIMATE, REALM, TAXA, ABUNDANCE_TYPE, BIOMASS_TYPE, 
                LATITUDE, LONGITUDE, CENT_LAT, CENT_LONG, NUMBER_LAT_LONG, SUMMARY_METHODS)
colnames(grid)[6:8] <- c('Species', 'Abundance', 'Biomass')

grid<-grid%>%group_by(STUDY_ID)%>%filter(n_distinct(YEAR)>=data_pt_thrs) %>% ungroup()
grid_terres<-grid%>%filter(REALM=="Terrestrial")
#===================== generate results folder for terrestrial ===============
resloc<-"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
saveRDS(grid_terres,paste(resloc,"bt_terres_min20yr_rawdata.RDS",sep=""))
grid_terres<-readRDS("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/bt_terres_min20yr_rawdata.RDS")
#============================================================================
# now watch each STUDY_ID
site<-sort(unique(grid_terres$STUDY_ID))
df<-data.frame(site=site)
df$nyr<-NA
df$nPLOT<-NA
df$NUMBER_LAT_LONG<-NA
df$LATmin<-NA
df$LATmax<-NA
df$LONmin<-NA
df$LONmax<-NA
df$monthlyfreqsamp<-NA
df$n_methods<-NA

library(htmltools) 
library(htmlwidgets)
library(leaflet) 

for(i in 1:nrow(df)){
  dat<-grid_terres%>%filter(STUDY_ID==site[i])
  df$nyr[i]<-length(unique(dat$YEAR))
  df$nPLOT[i]<-list(unique(dat$PLOT))
  df$NUMBER_LAT_LONG[i]<-unique(dat$NUMBER_LAT_LONG)
  df$LATmin[i]<-min(unique(dat$LATITUDE))
  df$LATmax[i]<-max(unique(dat$LATITUDE))
  df$LONmin[i]<-min(unique(dat$LONGITUDE))
  df$LONmax[i]<-max(unique(dat$LONGITUDE))
  t1<-dat%>%group_by(YEAR)%>%summarise(n_distinct(MONTH))%>%ungroup()
  df$monthlyfreqsamp[i]<-list(range(t1$`n_distinct(MONTH)`))
  df$n_methods[i]<-list(unique(dat$SUMMARY_METHODS))
  
  #---------- save sampling sites on map ----------
  dat<-dat%>%select(STUDY_ID,LATITUDE,LONGITUDE)%>%distinct()
  
  sitemap<-leaflet(dat) %>% addTiles() %>%
    addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(STUDY_ID))
  f<-paste("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/samplingsite_",
           dat$STUDY_ID[1],".html",sep="")
  htmlwidgets::saveWidget(sitemap, 
                          file.path(normalizePath(dirname(f)),basename(f)))
}
saveRDS(df,"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")

#-------- create res folder ------------
resloc2<-"../../Results/for_BioTIME/Terrestrial_plotlevel/"
if(!dir.exists(resloc2)){
  dir.create(resloc2)
}

#-----------------------------
source("terrestrial_plotlevel_18.R")
source("terrestrial_plotlevel_39.R")
source("terrestrial_plotlevel_42.R")
source("terrestrial_plotlevel_46.R")
source("terrestrial_plotlevel_47.R")
source("terrestrial_plotlevel_54.R")
source("terrestrial_plotlevel_56.R")
source("terrestrial_plotlevel_59.R")
source("terrestrial_plotlevel_63.R") # dragonfly: lake ecosystem, still terrestrial?






