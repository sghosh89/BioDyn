rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_zoop<-readRDS("../../Results/for_zoop_2014/summary_table.RDS")
r_zoop$ens<-NA # effective number of species
r_zoop$cvsq_real<-NA # square of CV for the real community data
r_zoop$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_zoop$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_zoop$phi_LdM<-NA # Loreau's variance ratio
r_zoop$skw_real <-NA # skewness for the real community data
r_zoop$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_zoop$phi_skw<-NA # skewness ratio
r_zoop$iCV<-NA # inverse of CV: stability metric

for(i in 1:nrow(r_zoop)){
  mypath<-paste("../../DATA/for_zoop_2014/wrangled_data/",r_zoop$siteid[i],"/",sep="")
  m<-readRDS(paste(mypath,"inputmat_for_tailanal.RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_zoop$ens[i]<-df$ens
  r_zoop$cvsq_real[i]<-df$cvsq_real
  r_zoop$cvsq_indep[i]<-df$cvsq_indep
  r_zoop$phi[i]<-df$phi
  r_zoop$phi_LdM[i]<-df$phi_LdM
  r_zoop$skw_real[i]<-df$skw_real
  r_zoop$skw_indep[i]<-df$skw_indep
  r_zoop$phi_skw[i]<-df$phi_skw
  r_zoop$iCV[i]<-df$iCV
}
r_zoop$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_zoop,"../../Results/for_zoop_2014/stability_metric.RDS")
