# This file will make a species list (only for dominant sp/ group considered in tail-analysis study)
# from all database used for this study

rm(list=ls())
library(tidyverse)
#=====================
df<-read.csv("../../Results/gather_res/data_summary.csv") # saved from datasummary.R
rownames(df)<-NULL

targetspecieslist_alldata<-c()
  
  #data.frame(source=NA*numeric(10^7),
 #                                     siteid=NA*numeric(10^7),
#                                      REALM=NA*numeric(10^7),
#                                      TAXA=NA*numeric(10^7),
#                                      ORGANISMS=NA*numeric(10^7),
#                                      nsp=NA*numeric(10^7),
#                                      species=NA*numeric(10^7))
#init<-0
for(i in 1:nrow(df)){
  
  rlm<-df$REALM[i]
  siteid<-df$siteid[i]
  source<-df$source[i]
  taxa<-df$TAXA[i]
  org<-df$ORGANISMS[i]
  nsp<-df$nsp[i]
  
  if(source=="BioTIME"){
    mypath<-paste("../../Results/for_BioTIME/",rlm,"/",siteid,"/",sep="")
  }
  
  if(source=="BioTIMEx"){
    
    if(siteid%in%c("carpenter_2016","gross_2016","oneida_phytopl_1975")){
      mypath<-paste("../../Results/for_BioTIMEx/",siteid,"/",sep="")
    }else if(siteid=="landis_2018"){
      mypath<-paste("../../Results/for_BioTIMEx/",siteid,"/poplarT5/",sep="")
    }else if(siteid=="lightfoot_2015_BOER_E"){
      mypath<-paste("../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E/",sep="")
    }else if(siteid=="lightfoot_2015_BOER_L"){
      mypath<-paste("../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_L/",sep="")
    }else if(siteid=="lightfoot_2015_LATR_E"){
      mypath<-paste("../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_E/",sep="")
    }else if(siteid=="lightfoot_2015_LATR_L"){
      mypath<-paste("../../Results/for_BioTIMEx/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_L/",sep="")
    }else{
      cat("---------- Error: missmatched name for BioTIMEx -------------- \n")
    }
    
  }
  
  if(source=="BBS"){
    mypath<-paste("../../Results/for_BBS/",siteid,"/",sep="")
  }
  
  if(source=="RivFishTIME"){
    mypath<-paste("../../Results/for_RivFishTIME/",siteid,"/",sep="")
  }
  
  if(source=="SwissLakePhyto"){
    mypath<-paste("../../Results/for_swisslake/",siteid,"/",sep="")
  }
  
  if(source=="SwissLakeZoo"){
    mypath<-paste("../../Results/for_swisslake/zooplankton/zoo_",siteid,"/",sep="")
  }
  
  if(source=="Zooplankton2014"){
    mypath<-paste("../../Results/for_zoop_2014/",siteid,"/",sep="")
  }
  
  cat(" i = ",i," source = ", source, " siteid = ", siteid, "\n")
  
  xx<-readRDS(paste(mypath,"NonParamStat.RDS",sep=""))
  xx<-xx$spear
  xx<-rownames(xx)
  xx<-setdiff(xx,c("raresp","covsp"))
  
  if(source=="BBS"){ #replace AOU code for birds with scientific names
    zall<-read.csv("../../DATA/for_BBS/wrangled_data/species_with_taxainfo_diet_edited.csv")
    xx<-zall$ScientificName[match(xx,zall$AOU)]
  }
  
  tgsp<-data.frame(source=NA*numeric(10^3),
                                        siteid=NA*numeric(10^3),
                                        REALM=NA*numeric(10^3),
                                        TAXA=NA*numeric(10^3),
                                        ORGANISMS=NA*numeric(10^3),
                                        nsp=NA*numeric(10^3),
                                        species=NA*numeric(10^3))
  lxx<-length(xx)
  tgsp$source[1:lxx]<-source
  tgsp$REALM[1:lxx]<-rlm
  tgsp$siteid[1:lxx]<-siteid
  tgsp$TAXA[1:lxx]<-taxa
  tgsp$ORGANISMS[1:lxx]<-org
  tgsp$nsp[1:lxx]<-nsp
  tgsp$species[1:lxx]<-xx
  
  tgsp<-na.omit(tgsp)
  targetspecieslist_alldata<-rbind(targetspecieslist_alldata,tgsp)
}

write.csv(targetspecieslist_alldata,"../../Results/gather_res/targetspecies_alldata.csv",row.names = F)











