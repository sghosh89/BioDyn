rm(list=ls())
source("call_model_for_eachtaxa.R")
library(brms)
library(tidyverse)
library(performance)

if(!dir.exists("../../Results/gather_res/test_eachtaxa")){
  dir.create("../../Results/gather_res/test_eachtaxa")
}


#========================================================================================
sink("../../Results/gather_res/test_eachtaxa/console_practice_hierarchicalmodel_taxawise.txt", append=TRUE, split=TRUE)
#====================================================================================================

#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$f_nL+mydat$f_nU # total asymmetry
mydat$uniA<-mydat$f_nL-mydat$f_nU # net asymmetry

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
# always large in number than freshwater
class(mydat_scaled$REALM)
mydat$TAXA<-as.factor(mydat$TAXA)
mydat$UID<-as.factor(mydat$UID)
#============================================================================
# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="fish")
call_model_for_eachtaxa(mydat=mydat,taxa="birds")

# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="freshwater invertebrates")
call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial invertebrates")

# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="freshwater plants")
call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial plants")

# terrestrial mammals
call_model_for_eachtaxa(mydat=mydat,taxa="mammals")

sink()







