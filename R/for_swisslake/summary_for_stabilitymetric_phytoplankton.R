rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_phyto<-readRDS("../../Results/for_swisslake/summary_table_phytoplankton.RDS")
r_phyto$ens<-NA # effective number of species
r_phyto$cvsq_real<-NA # square of CV for the real community data
r_phyto$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_phyto$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_phyto$phi_LdM<-NA # Loreau's variance ratio
r_phyto$skw_real <-NA # skewness for the real community data
r_phyto$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_phyto$phi_skw<-NA # skewness ratio
r_phyto$iCV<-NA # inverse of CV: stability metric
r_phyto$iCValt<-NA # inverse of CV alternative for skewed dist.: stability metric

for(i in 1:nrow(r_phyto)){
  siteid<-r_phyto$siteid[i]
  m<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/input_mat_for_tail_analysis_",siteid,".RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_phyto$ens[i]<-df$ens
  r_phyto$cvsq_real[i]<-df$cvsq_real
  r_phyto$cvsq_indep[i]<-df$cvsq_indep
  r_phyto$phi[i]<-df$phi
  r_phyto$phi_LdM[i]<-df$phi_LdM
  r_phyto$skw_real[i]<-df$skw_real
  r_phyto$skw_indep[i]<-df$skw_indep
  r_phyto$phi_skw[i]<-df$phi_skw
  r_phyto$iCV[i]<-df$iCV
  r_phyto$iCValt[i]<-df$iCValt
}
r_phyto$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_phyto,"../../Results/for_swisslake/stability_metric_for_phytoplankton.RDS")
