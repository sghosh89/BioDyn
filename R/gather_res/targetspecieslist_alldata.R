# This file will make a species list (only for dominant sp/ group considered in tail-analysis study)
# from all database used for this study

rm(list=ls())
library(tidyverse)
#=====================
df<-read.csv("../../Results/gather_res/data_summary.csv") # saved from datasummary.R
rownames(df)<-NULL

targetspecieslist_alldata<-c()
 
for(i in 1:nrow(df)){
  
  tempo<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
  rlm<-df$REALM[i]
  newsite<-df$newsite[i]
  source<-df$source[i]
  taxa<-df$TAXA[i]
  org<-df$ORGANISMS[i]
  nsp<-df$nsp[i]
  
  if(source=="BioTIME"){
    if(df$STUDY_ID[i]==df$newsite[i]){
      mypath<-paste("../../Results/for_BioTIME/",rlm,"_plotlevel/",newsite,"/",sep="")
    }else{
      mypath<-paste("../../Results/for_BioTIME/",rlm,"_plotlevel/",df$STUDY_ID[i],"/",newsite,"/",sep="")
    }
    
  }
  
  if(source=="BioTIMEx"){
    mypath<-tempo[which(newsite==newsite),]
    mypath<-mypath$resloc
  }
  
  if(source=="BBS"){
    mypath<-paste("../../Results/for_BBS/",newsite,"/",sep="")
  }
  
  if(source=="RivFishTIME"){
    mypath<-paste("../../Results/for_RivFishTIME/",newsite,"/",sep="")
  }
  
  if(source=="SwissLakePhyto"){
    mypath<-paste("../../Results/for_swisslake/",newsite,"/",sep="")
  }
  
  if(source=="SwissLakeZoo"){
    mypath<-paste("../../Results/for_swisslake/zooplankton/zoo_",newsite,"/",sep="")
  }
  
  if(source=="Zooplankton2014"){
    mypath<-paste("../../Results/for_zoop_2014/",newsite,"/",sep="")
  }
  
  cat(" i = ",i," source = ", source, " newsite = ", newsite, "\n")
  
  xx<-readRDS(paste(mypath,"NonParamStat.RDS",sep=""))
  xx<-xx$spear
  xx<-rownames(xx)
  xx<-setdiff(xx,c("raresp","covsp"))
  
  if(source=="BBS"){ #replace AOU code for birds with scientific names
    zall<-read.csv("../../DATA/for_BBS/wrangled_data/species_with_taxainfo_diet_edited.csv")
    xx<-zall$ScientificName[match(xx,zall$AOU)]
  }
  
  tgsp<-data.frame(source=NA*numeric(10^3),
                                        newsite=NA*numeric(10^3),
                                        REALM=NA*numeric(10^3),
                                        TAXA=NA*numeric(10^3),
                                        ORGANISMS=NA*numeric(10^3),
                                        nsp=NA*numeric(10^3),
                                        species=NA*numeric(10^3))
  lxx<-length(xx)
  tgsp$source[1:lxx]<-source
  tgsp$REALM[1:lxx]<-rlm
  tgsp$newsite[1:lxx]<-newsite
  tgsp$TAXA[1:lxx]<-taxa
  tgsp$ORGANISMS[1:lxx]<-org
  tgsp$nsp[1:lxx]<-nsp
  tgsp$species[1:lxx]<-xx
  
  tgsp<-na.omit(tgsp)
  targetspecieslist_alldata<-rbind(targetspecieslist_alldata,tgsp)
}

write.csv(targetspecieslist_alldata,"../../Results/gather_res/targetspecies_alldata.csv",row.names = F)











