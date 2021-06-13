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
source("terrestrial_plotlevel_67.R")
#source("terrestrial_plotlevel_195.R") # STUDY_ID=195 is BBS data - so we excluded here
source("terrestrial_plotlevel_214.R")
source("terrestrial_plotlevel_215.R")
# source("terrestrial_plotlevel_221.R") # not a single lat-lon sampled atleast for 20 yrs, if want to include this study aggregate all
source("terrestrial_plotlevel_243.R")
#source("terrestrial_plotlevel_298.R") # not a single lat-lon sampled atleast for 20 yrs, if want to include this study aggregate all
# source("terrestrial_plotlevel_300.R") # STUDY_ID=300 is BioTIMEx data for landis_2018 - so we excluded here
source("terrestrial_plotlevel_301.R")
source("terrestrial_plotlevel_308.R")
source("terrestrial_plotlevel_311.R")
source("terrestrial_plotlevel_333.R")
source("terrestrial_plotlevel_339.R")
source("terrestrial_plotlevel_355.R")
#source("terrestrial_plotlevel_356.R")# not a single lat-lon sampled atleast for 20 yrs, if want to include this study aggregate all
#source("terrestrial_plotlevel_360.R")# all raresp, warnings!
source("terrestrial_plotlevel_361.R")
source("terrestrial_plotlevel_363.R")
source("terrestrial_plotlevel_366.R")
source("terrestrial_plotlevel_413.R")
#source("terrestrial_plotlevel_414.R") # same as 413? but different time span?
#source("terrestrial_plotlevel_416.R") # same as 413? but different time span?
source("terrestrial_plotlevel_420.R")
#source("terrestrial_plotlevel_483.R") # all raresp, warnings!
#source("terrestrial_plotlevel_497.R") # all raresp, warnings!

df<-readRDS("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")
df_included<-df%>%filter(site%notin%c(195,221,298,300,356,360,414,416,483,497))
saveRDS(df_included,"../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map_selected.RDS")

#--------------- Do a summary stats for terrestrial sites ------------------
summary_table<-c()
for (i in c(1:length(df_included$site))){
  resloc<-paste("../../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/",df_included$site[i],"/",sep="")
  newsitelist<-readRDS(paste(resloc,"newsite.RDS",sep=""))
  
  if(length(newsitelist)==1){
    tempo<-newsitelist==df_included$site[i]
    if(tempo==T){
      resloc2<-paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",df_included$site[i],"/",sep="")
    }else{
      resloc2<-paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",df_included$site[i],"/",newsitelist,"/",sep="")
    }
    st<-readRDS(paste(resloc2,"summary_df.RDS",sep=""))
    st$STUDY_ID<-df_included$site[i]
    st$newsite<-newsitelist
    summary_table<-rbind(summary_table,st)
  }else{
    for(j in 1:length(newsitelist)){
      resloc2<-paste("../../Results/for_BioTIME/Terrestrial_plotlevel/",df_included$site[i],"/",newsitelist[j],"/",sep="")
      st<-readRDS(paste(resloc2,"summary_df.RDS",sep=""))
      st$STUDY_ID<-df_included$site[i]
      st$newsite<-newsitelist[j]
      summary_table<-rbind(summary_table,st)
    }
  }
}
# reorganize
summary_table<-summary_table%>%select(STUDY_ID,newsite,nsp,nint,nind,npos,nL,nU,nneg,L,U)

summary_table<-summary_table%>%mutate(f_nind=nind/nint,
                                      f_npos=npos/nint,
                                      f_nL=nL/nint,
                                      f_nU=nU/nint,
                                      f_nneg=nneg/nint)
saveRDS(summary_table,"../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")

df<-summary_table%>%select(STUDY_ID,newsite,nsp,f_nind,f_nL,f_nU,f_nneg)
xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
xxm<-xxm%>%select(STUDY_ID,TAXA)
df<-inner_join(df,xxm,"STUDY_ID")
df <-df[order(df$TAXA),]
dat<-t(df)
colnames(dat)<-dat[1,]
dat<-dat[-1,]
nsp<-dat[2,]
dat<-dat[-c(1:2),]
data_pt_thrs<-20 # 20 years minimum

pdf("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_plot.pdf",width=85,height=10)
op<-par(mar=c(12,5,5,1))
x<-barplot(dat,main = paste("Terrestrial dynamics: min ",data_pt_thrs," yrs",sep=""),
           xlab = "",ylab="Freq. of pairwise interaction",ylim=c(0,1.4),
           cex.lab=2,cex.main=2,names.arg = dat[5,],las=2,
           col = c("yellow","red","skyblue","green"))
text(x = x, y = 1, label = paste(colnames(dat),"(",nsp,")",sep=""), pos = 3, cex = 1, col = "purple")
#text(x = x, y = 1, label = colnames(dat), pos = 1, cex = 1.5, col = "purple")
legend("top",horiz=T,bty="n",cex=1.2,
       c("Independent","Synchrony when rare", "Synchrony when abundant","compensatory","site(species)"),
       fill = c("yellow","red","skyblue","green","purple"))
par(op)
dev.off()

##########################################################################

















