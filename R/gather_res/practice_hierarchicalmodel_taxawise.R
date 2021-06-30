rm(list=ls())
source("call_model_for_eachtaxa.R")
library(brms)
library(tidyverse)

#========================================================================================
sink("./console_practice_hierarchicalmodel_taxawise.txt", append=TRUE, split=TRUE)
#====================================================================================================

#===========================
df<-readRDS("../../Results/gather_res/stability_metric_all.RDS")
mydat<-df[,c("source","STUDY_ID","newsite","REALM","TAXA","ORGANISMS","iCV","iCValt","phi","phi_skw","nsp","L","U","f_nL","f_nU")]
mydat$UID<-paste(mydat$source,mydat$STUDY_ID,sep=",")
mydat$A<-mydat$f_nL+mydat$f_nU # total asymmetry
mydat$uniA<-mydat$f_nL-mydat$f_nU # net asymmetry
mydat$community<-"NT" #Symmetric
mydat$community[which(mydat$L>abs(mydat$U))]<-"LT"
mydat$community[which(mydat$L<abs(mydat$U))]<-"UT"

mydat<-mydat%>%rename(
  stability=iCV,
  stability_skw=iCValt,
  VR=phi,
  SR=phi_skw,
  R=nsp)
mydat<-na.omit(mydat) # one RivFishTIME data point omitted as SR=NaN

#============================================================================
table(mydat$TAXA) # mammals for terrestrial, terrestrial data are 
# always large in number than freshwater
#============================================================================
# freshwater & terrestrial
res<-call_model_for_eachtaxa(mydat=mydat,taxa="fish")
summary(res$nm)
summary(res$fm)
res<-call_model_for_eachtaxa(mydat=mydat,taxa="birds")
summary(res$nm)
summary(res$fm)

# freshwater & terrestrial
res<-call_model_for_eachtaxa(mydat=mydat,taxa="freshwater invertebrates")
summary(res$nm)
summary(res$fm)
res<-call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial invertebrates")
summary(res$nm)
summary(res$fm)


# freshwater & terrestrial
res<-call_model_for_eachtaxa(mydat=mydat,taxa="freshwater plants")
summary(res$nm)
summary(res$fm)
res<-call_model_for_eachtaxa(mydat=mydat,taxa="terrestrial plants")
summary(res$nm)
summary(res$fm)


# terrestrial mammals
res<-call_model_for_eachtaxa(mydat=mydat,taxa="mammals")
summary(res$nm)
summary(res$fm)

sink()







