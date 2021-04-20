rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)
# read summary results from freshwater, marine, terrestrial
r_BioTIME<-readRDS("../../Results/for_BioTIME/summary_table.RDS")
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

for(i in 1:nrow(r_BioTIME)){
  mypath<-paste("../../Results/for_BioTIME/",r_BioTIME$REALM[i],"/",r_BioTIME$siteid[i],"/",sep="")
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
saveRDS(r_BioTIME,"../../Results/for_BioTIME/stability_metric.RDS")
