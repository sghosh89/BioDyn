rm(list=ls())
source("get_ancova_res.R")

sm_all<-readRDS("../../Results/gather_res/stability_metric_all.RDS")

#-----------------------------------------------------------------
# omit marine points, also levels from factor REALM
sm_all$REALM<-as.character(sm_all$REALM)
sm_all<-sm_all[which(sm_all$REALM!="Marine"),]
sm_all$REALM<-as.factor(sm_all$REALM)
#----------------------------------------------------------------
sm_all<-na.omit(sm_all)

#sm_all<-sm_all%>%filter(source %in% c("Zooplankton2014","BioTIME","BioTIMEx","RivFishTIME"))

# check Synchrony-stability relationship: expected to be -ve
mydat<-sm_all[,c("phi","iCV","REALM")]

#-----------------------------------------------------
mydat$log_phi<-log(mydat$phi)
mydat$log_iCV<-log(mydat$iCV)
class(mydat$REALM) # should be factor 

res<-get_ancova_res(mydat=mydat,xlab="log_phi",ylab="log_iCV")
res$plot_linear
res$res.aov.tab
res$pwc
res$pwc_vis


# problem: assumption not met for ancova, what to do
#######################################################

# Stability-richness relationship
mydat<-sm_all[,c("nsp","iCV","REALM")]
mydat$log_nsp<-log(mydat$nsp)
mydat$log_iCV<-log(mydat$iCV)
class(mydat$REALM) # should be factor 

res<-get_ancova_res(mydat=mydat,xlab="log_nsp",ylab="log_iCV")
res$plot_linear
res$res.aov.tab
res$pwc
res$pwc_vis

# Synchrony-richness relationship: no relationship, not as a whole (using phi_LdM), not between groups
mydat<-sm_all[,c("nsp","phi","REALM")]
mydat$log_nsp<-log(mydat$nsp)
mydat$log_phi<-log(mydat$phi)
class(mydat$REALM) # should be factor 

res<-get_ancova_res(mydat=mydat,xlab="log_nsp",ylab="log_phi")
res$plot_linear
res$res.aov.tab
res$pwc
res$pwc_vis

#--------------------------------------------------
mydat<-sm_all[,c("iCV","phi","REALM","phi_skw","nsp","L","U")]
mydat$A<-mydat$L+abs(mydat$U) # total asymmetry
mydat$uniA<-mydat$L+mydat$U
mydat$AL<-mydat$L/mydat$A # fraction of L in total Asym
mydat$AU<-abs(mydat$U)/mydat$A
mydat$community<-ifelse((mydat$uniA) >0,"LT","UT") # LT==UT wrongly predicted as UT here
mydat<-mydat%>%filter(phi_skw>0)
table(mydat$REALM)
fr<-mydat%>%filter(REALM=="Freshwater")
sum(fr$AL>fr$AU,na.rm=T)# LT
sum(fr$AL<fr$AU,na.rm=T) # UT
tr<-mydat%>%filter(REALM=="Terrestrial")
sum(tr$AL>tr$AU,na.rm=T)# LT
sum(tr$AL<tr$AU,na.rm=T) # UT

mydat$log_phi<-log(mydat$phi)
mydat$log_phi_skw<-log(mydat$phi_skw)

res<-get_ancova_res(mydat=mydat,xlab="log_phi",ylab="log_phi_skw")
res$plot_linear
res$res.aov.tab
res$pwc
res$pwc_vis
# this map explain stability intuition log scale phiskw +ve: should be less stable as UT dep.
# then why 














