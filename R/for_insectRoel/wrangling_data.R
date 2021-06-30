# This script will wrangle the data for each community and 
# save the raw data and wrangled data in respective DATA folder
#====================================================================
rm(list=ls())
source("./get_communitylevel_data.R")
#--------- read data ---------------------------------
xm<-read.csv("../../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
#x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData.rds")
x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData 202106.rds")
#xn<-dplyr::anti_join(x,x2,by="Plot_ID")
#x<-x%>%filter(Number>0)

Datasource_ID<-setdiff(sort(unique(x$Datasource_ID)),63)

#------------------ first create DATASOURCE_ID folder ----------------
dataloc<-"../../DATA/for_insectRoel/wrangled_data/"
if(!dir.exists(dataloc)){
  dir.create(dataloc)
}
for(i in 1:length(Datasource_ID)){
  tempo<-paste("../../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/",sep="")
  if(!dir.exists(tempo)){
    dir.create(tempo)
  }
}
#----------------------------------------------------------------------
for(i in 1:length(Datasource_ID)){
  did<-Datasource_ID[i]
  xd<-x%>%filter(Datasource_ID==did)
  Plot_ID<-sort(unique(xd$Plot_ID))
  saveRDS(Plot_ID,paste("../../DATA/for_insectRoel/wrangled_data/",did,"/Plot_ID_list.RDS",sep=""))
  for(j in 1:length(Plot_ID)){
    pid<-Plot_ID[j]
    xdp<-xd%>%filter(Plot_ID==pid)
    tempo<-paste("../../DATA/for_insectRoel/wrangled_data/",did,"/",pid,"/",sep="")
    if(!dir.exists(tempo)){
      dir.create(tempo)
    }
    #saveRDS(xdp,paste(tempo,"rawdata_community.RDS",sep=""))
  }
}
#-------------------- Now do the wrangling ----------------------------
for(i in 1:length(Datasource_ID)){
  did<-Datasource_ID[i]
  Plot_ID<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",did,"/Plot_ID_list.RDS",sep=""))
  bad_pidlist<-c()
  for(j in 1:length(Plot_ID)){
    pid<-Plot_ID[j]
    cc<-x%>%filter(Datasource_ID==did & Plot_ID==pid)
    resloc<- paste("../../DATA/for_insectRoel/wrangled_data/",did,"/",pid,"/",sep="") 
    cm<-get_communitylevel_data(cc=cc,resloc=resloc)
    if(is.na(cm)==T){
      bad_pidlist<-c(bad_pidlist,pid)
    }
    cat("i = ", i, ", j = ",j,", did = ",did, ", pid = ",pid,"\n")
  }
  saveRDS(bad_pidlist,paste("../../DATA/for_insectRoel/wrangled_data/",did,"/bad_pidlist.RDS",sep=""))
}
  

