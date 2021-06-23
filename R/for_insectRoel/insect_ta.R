rm(list=ls())
#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
#--------- read data ---------------------------------
xm<-read.csv("../../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
x<-readRDS("../../DATA/for_insectRoel/20yrFreshwaterData.rds")
#x<-x%>%filter(Number>0)

#---------------------------------------
resloc<-"../../Results/for_insectRoel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

Datasource_ID<-setdiff(sort(unique(x$Datasource_ID)),63)
for(i in 1:length(Datasource_ID)){
  did<-paste(resloc,Datasource_ID[i],"/",sep="")
  if(!dir.exists(did)){
    dir.create(did)
  }
  pidlist<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/Plot_ID_list.RDS",sep=""))
  badpidlist<-readRDS(paste("../../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/bad_pidlist.RDS",sep=""))
  goodpidlist<-setdiff(pidlist,badpidlist)
  saveRDS(goodpidlist,paste("../../Results/for_insectRoel/",Datasource_ID[i],"/goodpidlist.RDS",sep=""))
  for(j in 1:length(goodpidlist)){
    pid<-paste(did,goodpidlist[j],"/",sep="")
    if(!dir.exists(pid)){
      dir.create(pid)
    }
  }
}

#----------- Now compute and plot the tail stats ---------------------
  
for(i in 1:length(Datasource_ID)){
    did<-Datasource_ID[i]
    goodpidlist<-readRDS(paste("../../Results/for_insectRoel/",did,"/goodpidlist.RDS",sep=""))
    for(j in 1:length(goodpidlist)){
      pid<-goodpidlist[j]
      resloc_output<-paste(resloc,did,"/",pid,"/",sep="")
      resloc_input<-paste("../../DATA/for_insectRoel/wrangled_data/",did,"/",pid,"/",sep="")
      
      df<-readRDS(paste(resloc_input,"inputmat_for_tailanal.RDS",sep="")) # dataframe with species timeseries along column
      #----------- analysis with covary sp ----------------
      if(ncol(df)>=2){
        res<-tail_analysis(mat = df, resloc = resloc_output, nbin = 2)
        cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," ----------\n")
      }else{
        cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," not enough sp present ----------\n")
      }
      
    }
}



