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

# This needs to be updated from inputmatfile_list of rank_category.R
inputmatfile_list<-c(
  "../../DATA/for_BioTIMEx/wrangled_data/carpenter_2016/carpenter_2016_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/gross_2016/gross_2016_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/landis_2018/landis_2018_inputmatrix_tailanal_poplarT5.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_L_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_E_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_L_inputmatrix_tailanal.RDS",
  "../../DATA/for_BioTIMEx/wrangled_data/oneida_phytopl_1975/oneida_phytopl_1975_inputmatrix_tailanal.RDS"
)

myrealm<-c("Freshwater","Terrestrial","Terrestrial",
           "Terrestrial","Terrestrial","Terrestrial","Terrestrial",
           "Freshwater")

for(i in 1:nrow(r_combo)){
  siteid<-r_combo$siteid[i]
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
}

r_combo$REALM<-as.factor(myrealm)
##########################################################
saveRDS(r_combo,"../../Results/for_BioTIMEx/stability_metric.RDS")
