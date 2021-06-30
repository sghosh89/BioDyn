rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)
# read summary results from freshwater, terrestrial
r_frs<-readRDS("../../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")
r_ter<-readRDS("../../Results/for_BioTIME/Terrestrial_plotlevel/summary_table.RDS")
r_ter<-r_ter%>%filter(f_nind!=1) # remove community with all independent interactions

r_BioTIME<-rbind(r_frs,r_ter)
r_BioTIME$ens<-NA # effective number of species
r_BioTIME$cvsq_real<-NA # square of CV for the real community data
r_BioTIME$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BioTIME$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BioTIME$phi_LdM<-NA # Loreau's variance ratio
r_BioTIME$skw_real <-NA # skewness for the real community data
r_BioTIME$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BioTIME$phi_skw<-NA # skewness ratio
r_BioTIME$iCV<-NA # inverse of CV: stability metric
r_BioTIME$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

xxm<-readRDS("../../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
xxm<-xxm%>%dplyr::select(STUDY_ID,REALM,TAXA,ORGANISMS)
r_BioTIME<-left_join(r_BioTIME,xxm,"STUDY_ID")

for(i in 1:nrow(r_BioTIME)){
  
  STUDY_ID<-r_BioTIME$STUDY_ID[i]
  newsite<-r_BioTIME$newsite[i]
  realm<-r_BioTIME$REALM[i]
  if(STUDY_ID==newsite){
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",sep="")
  }else{
    mypath<-paste("../../DATA/for_BioTIME/wrangled_data/",realm,"_plotlevel/",STUDY_ID,"/",newsite,"/",sep="")
  }
  
  m<-readRDS(paste(mypath,"input_tailanal.RDS",sep=""))
  
  df<-get_stability_metric(m=m)
  r_BioTIME$ens[i]<-df$ens
  r_BioTIME$cvsq_real[i]<-df$cvsq_real
  r_BioTIME$cvsq_indep[i]<-df$cvsq_indep
  r_BioTIME$phi[i]<-df$phi
  r_BioTIME$phi_LdM[i]<-df$phi_LdM
  r_BioTIME$skw_real[i]<-df$skw_real
  r_BioTIME$skw_indep[i]<-df$skw_indep
  r_BioTIME$phi_skw[i]<-df$phi_skw
  r_BioTIME$iCV[i]<-df$iCV
  r_BioTIME$iCValt[i]<-df$iCValt
}
r_BioTIME$REALM<-as.factor(r_BioTIME$REALM)
##########################################################
saveRDS(r_BioTIME,"../../Results/for_BioTIME/stability_metric_plotlevel.RDS")
