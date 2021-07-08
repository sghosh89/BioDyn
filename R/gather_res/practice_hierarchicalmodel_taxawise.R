rm(list=ls())
source("call_model_for_eachtaxa.R")
library(tidyverse)

resloc<-"../../Results/gather_res/test_eachtaxa"
if(!dir.exists(resloc)){
  dir.create(resloc)
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
class(mydat$REALM)
mydat$TAXA<-as.factor(mydat$TAXA)
mydat$UID<-as.factor(mydat$UID)
#============================================================================
# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="fish",resloc=resloc)
call_model_for_eachtaxa(mydat=mydat,taxa="birds",resloc=resloc)

# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="freshwater invertebrates",resloc=resloc)
call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial invertebrates",resloc=resloc)

# freshwater & terrestrial
call_model_for_eachtaxa(mydat=mydat,taxa="freshwater plants",resloc=resloc)
call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial plants",resloc=resloc)

# terrestrial mammals
call_model_for_eachtaxa(mydat=mydat,taxa="mammals",resloc=resloc)

sink()







