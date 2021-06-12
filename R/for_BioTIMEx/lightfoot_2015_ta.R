# This script will do the copula analysis, will generate necessary plots, will show how a species 
# tail dep. fluctuates with the rest of species in the phyto-plankton community

source("tail_analysis.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)

resloc<-'../../Results/for_BioTIMEx/lightfoot_2015/'
if(!dir.exists(resloc)){
  dir.create(resloc)
}

########################
#dataset_id<-"lightfoot_2015_site_BOER_sampling_time_E"
#myresloc<-paste(resloc,dataset_id,"/",sep="")
#if(!dir.exists(myresloc)){
#  dir.create(myresloc)
#}
#spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_BOER_sampling_time_E_inputmatrix_tailanal.RDS",sep=""))

#tail_analysis(mat=spmat,resloc=myresloc,nbin=2)

###########################
myresloc<-paste(resloc,"BOER/",sep="")
if(!dir.exists(myresloc)){
  dir.create(myresloc)
}
spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_sampling_time_L_site_BOER_inputmatrix_tailanal.RDS",sep=""))

tail_analysis(mat=spmat,resloc=myresloc,nbin=2)

########################
#dataset_id<-"lightfoot_2015_site_LATR_sampling_time_E"
#myresloc<-paste(resloc,dataset_id,"/",sep="")
#if(!dir.exists(myresloc)){
#  dir.create(myresloc)
#}
#spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_site_LATR_sampling_time_E_inputmatrix_tailanal.RDS",sep=""))

#tail_analysis(mat=spmat,resloc=myresloc,nbin=2)

###########################
myresloc<-paste(resloc,"LATR/",sep="")
if(!dir.exists(myresloc)){
  dir.create(myresloc)
}
spmat<-readRDS(paste("../../DATA/for_BioTIMEx/wrangled_data/lightfoot_2015/lightfoot_2015_sampling_time_L_site_LATR_inputmatrix_tailanal.RDS",sep=""))

tail_analysis(mat=spmat,resloc=myresloc,nbin=2)





