rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_combo<-readRDS("../../Results/for_BioTIMEx/summary_table.RDS")
r_combo$ens<-NA # effective number of species
r_combo$cvsq_real<-NA # square of CV for the real community data
r_combo$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_combo$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_combo$phi_LdM<-NA # Loreau's variance ratio
r_combo$skw_real <-NA # skewness for the real community data
r_combo$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_combo$phi_skw<-NA # skewness ratio
r_combo$iCV<-NA # inverse of CV: stability metric
r_combo$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# This needs to be updated from inputmatfile_list of rank_category.R
inputmatfile_list<-readRDS("../../Results/for_BioTIMEx/inputloc_table.RDS")
inputmatfile_list<-inputmatfile_list$inputloc

for(i in 1:nrow(r_combo)){
  m<-readRDS(inputmatfile_list[i])
  df<-get_stability_metric(m=m)
  r_combo$ens[i]<-df$ens
  r_combo$cvsq_real[i]<-df$cvsq_real
  r_combo$cvsq_indep[i]<-df$cvsq_indep
  r_combo$phi[i]<-df$phi
  r_combo$phi_LdM[i]<-df$phi_LdM
  r_combo$skw_real[i]<-df$skw_real
  r_combo$skw_indep[i]<-df$skw_indep
  r_combo$phi_skw[i]<-df$phi_skw
  r_combo$iCV[i]<-df$iCV
  r_combo$iCValt[i]<-df$iCValt
}

##########################################################
saveRDS(r_combo,"../../Results/for_BioTIMEx/stability_metric.RDS")
