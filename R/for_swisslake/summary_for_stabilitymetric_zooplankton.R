rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_zoo<-readRDS("../../Results/for_swisslake/summary_table_zooplankton.RDS")
r_zoo$ens<-NA # effective number of species
r_zoo$cvsq_real<-NA # square of CV for the real community data
r_zoo$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_zoo$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_zoo$phi_LdM<-NA # Loreau's variance ratio
r_zoo$skw_real <-NA # skewness for the real community data
r_zoo$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_zoo$phi_skw<-NA # skewness ratio
r_zoo$iCV<-NA # inverse of CV: stability metric
r_zoo$iCValt<-NA # inverse of CV alternative for skewed dist.: stability metric

for(i in 1:nrow(r_zoo)){
  siteid<-r_zoo$siteid[i]
  if(siteid=="LU"){
    siteid<-"LU_site3A01"
  }
  m<-readRDS(paste("../../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tail_analysis_zoo_",siteid,".RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_zoo$ens[i]<-df$ens
  r_zoo$cvsq_real[i]<-df$cvsq_real
  r_zoo$cvsq_indep[i]<-df$cvsq_indep
  r_zoo$phi[i]<-df$phi
  r_zoo$phi_LdM[i]<-df$phi_LdM
  r_zoo$skw_real[i]<-df$skw_real
  r_zoo$skw_indep[i]<-df$skw_indep
  r_zoo$phi_skw[i]<-df$phi_skw
  r_zoo$iCV[i]<-df$iCV
  r_zoo$iCValt[i]<-df$iCValt
}
r_zoo$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_zoo,"../../Results/for_swisslake/stability_metric_for_zooplankton.RDS")
