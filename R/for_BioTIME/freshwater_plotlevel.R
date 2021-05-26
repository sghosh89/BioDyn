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
grid_freshw<-grid%>%filter(REALM=="Freshwater")

#===================== generate results folder for freshwater ===============
resloc<-"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
saveRDS(grid_freshw,paste(resloc,"bt_freshw_min20yr_rawdata.RDS",sep=""))
grid_freshw<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/bt_freshw_min20yr_rawdata.RDS")
#============================================================================
# now watch each STUDY_ID
site<-unique(grid_freshw$STUDY_ID)
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
  dat<-grid_freshw%>%filter(STUDY_ID==site[i])
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
  f<-paste("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/samplingsite_",
           dat$STUDY_ID[1],".html",sep="")
  htmlwidgets::saveWidget(sitemap, 
                          file.path(normalizePath(dirname(f)),basename(f)))
}

#==============================================================================
# Ok, after seeing the maps we made some decision for each STUDY_ID

# for STUDY_ID = 229, each distinct lat-lon should be treated as separate plot 
# nested within a site=229

# for STUDY_ID = 238, we need to create 3 different STUDY_ID as the lakes are not connected

# for STUDY_ID = 247, 6 sampling sites nested sithin

# for STUDY_ID = 253, 5 separate lakes, not nested

# for STUDY_ID = 254, 2 separate lakes, not nested

# exclude following
# for STUDY_ID = 328, frog data, drop this data, sites are on lands
# for STUDY_ID = 426,427, Francesco already gave us Lake Zurich phyto & zoopl data

# for STUDY_ID = 430, 431, drop these data from New Zealand for now, 
#     to include these we need to define the hydrobasins and how the sampling sites
#      clumped into different watershades

#==============================================================================
df<-df[which(df$site%notin%c(328,426,427)),] # excluding the 328, 426, 427 site (amphibian)
saveRDS(df,"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")

#======================================================
# tail analysis 
source("./freshwater_plotlevel_57.R")
source("./freshwater_plotlevel_229.R") 
source("./freshwater_plotlevel_238.R")
source("./freshwater_plotlevel_247.R")
source("./freshwater_plotlevel_253.R")
source("./freshwater_plotlevel_254.R")
#source("./freshwater_plotlevel_430.R") #? each lat lon mostly visited once
#source("./freshwater_plotlevel_431.R") #? each lat lon mostly visited once
source("./freshwater_plotlevel_478.R")

df<-readRDS("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")
df_included<-df%>%filter(site%notin%c(430,431))
saveRDS(df_included,"../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map_selected.RDS")

#--------------- Do a summary stats for freshwater sites ------------------
summary_table<-c()
for (i in c(1:length(df_included$site))){
  resloc<-paste("../../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",df_included$site[i],"/",sep="")
  newsitelist<-readRDS(paste(resloc,"newsite.RDS",sep=""))
  
  if(length(newsitelist)==1){
    tempo<-newsitelist==df_included$site[i]
    if(tempo==T){
      resloc2<-paste("../../Results/for_BioTIME/Freshwater_plotlevel/",df_included$site[i],"/",sep="")
    }else{
      resloc2<-paste("../../Results/for_BioTIME/Freshwater_plotlevel/",df_included$site[i],"/",newsitelist,"/",sep="")
    }
    st<-readRDS(paste(resloc2,"summary_df.RDS",sep=""))
    st$STUDY_ID<-df_included$site[i]
    st$newsite<-newsitelist
    summary_table<-rbind(summary_table,st)
  }else{
    for(j in 1:length(newsitelist)){
      resloc2<-paste("../../Results/for_BioTIME/Freshwater_plotlevel/",df_included$site[i],"/",newsitelist[j],"/",sep="")
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
saveRDS(summary_table,"../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")

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

pdf("../../Results/for_BioTIME/Freshwater_plotlevel/summary_plot.pdf",width=25,height=10)
op<-par(mar=c(12,5,5,1))
x<-barplot(dat,main = paste("Freshwater dynamics: min ",data_pt_thrs," yrs",sep=""),
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










