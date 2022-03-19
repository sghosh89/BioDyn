# This script will make 100 random datasets, it will draw 15 datapoints 
# (as it's the minimum for phytoplankton) from each taxa
# 
rm(list=ls())
library(tidyverse)
set.seed(seed=123)
#====================================================================================================
# read data
df<-readRDS("../../Results/gather_res/stability_metric_all_subset_birds_21.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_LdM","nsp","L","U","f_nL","f_nU","f_nneg")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U # net asymmetry

mydat<-mydat%>%dplyr::rename(
  stability_skw=iCValt,
  VR_LdM=phi_LdM,
  R=nsp)

mydat<-mydat%>%select(REALM,TAXA,UID,stability_skw,R,VR_LdM,A)
#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
# always large in number than freshwater
# we will subset 15 data points from each taxon

mydat$TAXA<-as.factor(mydat$TAXA)
mydat$UID<-as.factor(mydat$UID)

if(!dir.exists("../../Results/gather_res/res_taxa15")){
  dir.create("../../Results/gather_res/res_taxa15")
}

for(i in 1:100){
  new_df <- mydat %>% group_by(TAXA) %>% dplyr::slice_sample(n=15,replace=F)
  if(!dir.exists(paste("../../Results/gather_res/res_taxa15/run_",i,sep=""))){
    dir.create(paste("../../Results/gather_res/res_taxa15/run_",i,sep=""))
    saveRDS(new_df,paste("../../Results/gather_res/res_taxa15/run_",i,"/reduced_data_taxa15.RDS",sep=""))
  }
}

#############################################

