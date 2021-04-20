rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_fish<-readRDS("../../Results/for_RivFishTIME/summary_table_detail_version.RDS")
r_fish$ens<-NA # effective number of species
r_fish$cvsq_real<-NA # square of CV for the real community data
r_fish$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_fish$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_fish$phi_LdM<-NA # Loreau's variance ratio
r_fish$skw_real <-NA # skewness for the real community data
r_fish$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_fish$phi_skw<-NA # skewness ratio
r_fish$iCV<-NA # inverse of CV: stability metric
r_fish$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

for(i in 1:nrow(r_fish)){
  siteid<-r_fish$siteid[i]
  m<-readRDS(paste("../../DATA/for_RivFishTIME/wrangled_data/",siteid,"/commonspecies_timeseries.RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_fish$ens[i]<-df$ens
  r_fish$cvsq_real[i]<-df$cvsq_real
  r_fish$cvsq_indep[i]<-df$cvsq_indep
  r_fish$phi[i]<-df$phi
  r_fish$phi_LdM[i]<-df$phi_LdM
  r_fish$skw_real[i]<-df$skw_real
  r_fish$skw_indep[i]<-df$skw_indep
  r_fish$phi_skw[i]<-df$phi_skw
  r_fish$iCV[i]<-df$iCV
  r_fish$iCValt[i]<-df$iCValt
}
r_fish$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_fish,"../../Results/for_RivFishTIME/stability_metric.RDS")
