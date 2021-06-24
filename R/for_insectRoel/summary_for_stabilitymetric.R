rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_insect<-readRDS("../../Results/for_insectRoel/summary_table_detail_version.RDS")
r_insect$ens<-NA # effective number of species
r_insect$cvsq_real<-NA # square of CV for the real community data
r_insect$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_insect$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_insect$phi_LdM<-NA # Loreau's variance ratio
r_insect$skw_real <-NA # skewness for the real community data
r_insect$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_insect$phi_skw<-NA # skewness ratio
r_insect$iCV<-NA # inverse of CV: stability metric
r_insect$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

for(i in 1:nrow(r_insect)){
  mypath<-paste("../../DATA/for_insectRoel/wrangled_data/",r_insect$STUDY_ID[i],"/",r_insect$newsite[i],"/",sep="")
  m<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_insect$ens[i]<-df$ens
  r_insect$cvsq_real[i]<-df$cvsq_real
  r_insect$cvsq_indep[i]<-df$cvsq_indep
  r_insect$phi[i]<-df$phi
  r_insect$phi_LdM[i]<-df$phi_LdM
  r_insect$skw_real[i]<-df$skw_real
  r_insect$skw_indep[i]<-df$skw_indep
  r_insect$phi_skw[i]<-df$phi_skw
  r_insect$iCV[i]<-df$iCV
  r_insect$iCValt[i]<-df$iCValt
}
r_insect$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_insect,"../../Results/for_insectRoel/stability_metric.RDS")