rm(list=ls())
source("./get_stability_metric.R")
library(tidyverse)
library(ggpmisc)

# read summary results 
r_BBS<-readRDS("../../Results/for_BBS/summary_table_detail_version.RDS")
r_BBS$ens<-NA # effective number of species
r_BBS$cvsq_real<-NA # square of CV for the real community data
r_BBS$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BBS$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BBS$phi_LdM<-NA # Loreau's variance ratio
r_BBS$skw_real <-NA # skewness for the real community data
r_BBS$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BBS$phi_skw<-NA # skewness ratio
r_BBS$iCV<-NA # inverse of CV: stability metric

for(i in 1:nrow(r_BBS)){
  mypath<-paste("../../DATA/for_BBS/wrangled_data/",r_BBS$siteid[i],"/",sep="")
  m<-readRDS(paste(mypath,"input_mat_for_tailanal.RDS",sep=""))
  df<-get_stability_metric(m=m)
  r_BBS$ens[i]<-df$ens
  r_BBS$cvsq_real[i]<-df$cvsq_real
  r_BBS$cvsq_indep[i]<-df$cvsq_indep
  r_BBS$phi[i]<-df$phi
  r_BBS$phi_LdM[i]<-df$phi_LdM
  r_BBS$skw_real[i]<-df$skw_real
  r_BBS$skw_indep[i]<-df$skw_indep
  r_BBS$phi_skw[i]<-df$phi_skw
  r_BBS$iCV[i]<-df$iCV
}
r_BBS$REALM<-as.factor("Terrestrial")
##########################################################
saveRDS(r_BBS,"../../Results/for_BBS/stability_metric.RDS")
